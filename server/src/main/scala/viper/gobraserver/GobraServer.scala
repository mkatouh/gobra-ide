// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.gobraserver

import viper.gobra.Gobra
import viper.gobra.GobraFrontend
import viper.gobra.reporting.VerifierResult
import viper.gobra.util.{GobraExecutionContext, Violation}
import viper.gobra.reporting.BackTranslator.BackTrackInfo
import viper.silver.ast.Program

import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.Source
import viper.server.core.ViperCoreServer
import org.eclipse.lsp4j.{MessageParams, MessageType, Range}
import viper.gobraserver.backend.ViperServerBackend
import viper.server.ViperConfig

import scala.concurrent.Future
import scala.util.{Failure, Success}


class GobraServerException extends Exception

case class GobraServerCacheInconsistentException() extends GobraServerException {
  override def toString: String = s"The diagnostics Cache is not consistent with Viper Server Cache."
}


object GobraServer extends GobraFrontend {


  private var _verifier: Gobra = _
  def verifier: Gobra = _verifier

  private var _options: List[String] = List()
  private var _executor: GobraServerExecutionContext = _
  private var _server: ViperCoreServer = _

  def init(options: List[String])(executor: GobraServerExecutionContext): Unit = {
    _options = options
    _executor = executor
    val config = new ViperConfig(options)
    _server = new ViperCoreServer(config)(executor)
    ViperServerBackend.setExecutor(_executor)
    ViperServerBackend.setServer(_server)
  }

  def start(): Unit = {
    _verifier = new Gobra
    _server.start()
    VerifierState.flushCachedDiagnostics()
  }

  def restart(): Future[Unit] = {
    stop()
      .flatMap(_ => {
        delete()
        _executor.restart()
      })(_executor)
      .map(_ => {
        val options = _options
        val executor = _executor
        init(options)(executor)
        start()
      })(_executor)
  }

  private def serverExceptionHandling(fileData: FileData, resultFuture: Future[VerifierResult])(implicit executor: GobraExecutionContext): Future[VerifierResult] = {

    val fileUri = fileData.fileUri

    // do some post processing if verification has failed
    resultFuture.transformWith {
      case Success(res) =>
        _server.logger.get.trace(s"GobraServer: verification was successful: $res")
        Future.successful(res)
      case Failure(exception) =>
        // restart Gobra Server and then update client state
        // ignore result of restart and inform the client:
        restart().transformWith(_ => {
          exception match {
            case e: Violation.LogicException =>
              VerifierState.removeDiagnostics(fileUri)
              val overallResult = Helper.getOverallVerificationResultFromException(fileUri, e)

              VerifierState.updateVerificationInformation(fileUri, Right(overallResult))


              if (fileUri == VerifierState.openFileUri) {
                VerifierState.publishDiagnostics(fileUri)
              }

            case e =>
              println("Exception occurred:")
              e.printStackTrace()

              // remove verification information about this file
              // otherwise, reopening this file in the client will result in sending the last progress although no
              // verification is running
              VerifierState.removeVerificationInformation(fileUri)

              VerifierState.client match {
                case Some(c) =>
                  c.showMessage(new MessageParams(MessageType.Error, "An exception occurred during verification: " + e))
                  c.verificationException(fileUri)
                case None =>
              }
          }
          // forward original result
          Future.failed(exception)
        })
    }
  }

  /**
    * Preprocess file and enqueue the Viper AST whenever it is created.
    */
  def preprocess(verifierConfig: VerifierConfig)(implicit executor: GobraExecutionContext): Future[VerifierResult] = {
    val fileUri = verifierConfig.fileData.fileUri

    VerifierState.verificationRunning += 1
    VerifierState.removeDiagnostics(fileUri)

    val startTime = System.currentTimeMillis()

    val config = Helper.verificationConfigFromTask(verifierConfig, startTime, verify = false, logger = _server.logger)(executor)
    val preprocessFuture = verifier.verify(config)(executor)

    serverExceptionHandling(verifierConfig.fileData, preprocessFuture)
  }

  /**
    * Preprocess go file and enqueue the Viper AST whenever it is created.
    */
  def preprocessGo(verifierConfig: VerifierConfig)(implicit executor: GobraExecutionContext): Future[VerifierResult] = {
    val fileUri = verifierConfig.fileData.fileUri
    val filePath = verifierConfig.fileData.filePath

    VerifierState.verificationRunning += 1
    VerifierState.removeDiagnostics(fileUri)

    val fileBuffer = Source.fromFile(filePath)
    val fileContents = fileBuffer.mkString
    fileBuffer.close()
    val gobrafiedContents = Gobrafier.gobrafyFileContents(fileContents)

    println(gobrafiedContents)

    val startTime = System.currentTimeMillis()

    val config = Helper.verificationConfigFromTask(verifierConfig, startTime, verify = false, logger = _server.logger)(executor)

    val tempFileName = s"gobrafiedProgram_${DateTimeFormatter.ofPattern("yyyy-MM-dd_HH_mm").format(LocalDateTime.now)}"
    val tempFi = File.createTempFile(tempFileName, ".gobra")
    new PrintWriter(tempFi) {
      try {
        write(gobrafiedContents)
      } finally {
        close()
      }
    }

    // adapt config to use the temp file instead of the original file containing the Go code
    val tmpConfig = config.copy(inputFiles = Vector(tempFi.toPath))
    val verifyAndDeleteTempFile = verifier.verify(tmpConfig)(executor)
      .transform(res => {
        // delete the temporary file (in case of success & failure)
        // note that this continuation does not run after the verification but already after desugaring (i.e. before inserting the Viper AST into the queue)
        // delete the temporary file is fine at this point because only the in-memory Viper AST is used for the subsequent steps
        val deleted = tempFi.delete()
        if (!deleted) {
          println(s"Deleting temporary file has failed (file: ${tempFi.getAbsolutePath})")
        }
        res
      })

    serverExceptionHandling(verifierConfig.fileData, verifyAndDeleteTempFile)
  }

  /**
    * Verify Viper AST.
    */
  def verify(verifierConfig: VerifierConfig, ast: Program, backtrack: BackTrackInfo, startTime: Long)(implicit executor: GobraExecutionContext): Future[VerifierResult] = {
    val completedProgress = (100 * (1 - Helper.defaultVerificationFraction)).toInt
    val config = Helper.verificationConfigFromTask(verifierConfig, startTime, verify = true, completedProgress, logger = _server.logger)(executor)

    val resultFuture = verifier.verifyAst(config, ast, backtrack)(executor)

    serverExceptionHandling(verifierConfig.fileData, resultFuture)
  }

  /**
    * Goify File and publish potential errors as Diagnostics.
    */
  def goify(fileData: FileData)(implicit executor: GobraExecutionContext): Future[VerifierResult] = {
    val fileUri = fileData.fileUri
    val config = Helper.goifyConfigFromTask(fileData)
    val goifyFuture = verifier.verify(config)(executor)

    goifyFuture.onComplete {
      case Success(result) =>
        (result, VerifierState.client) match {
          case (VerifierResult.Success, Some(c)) =>
            c.finishedGoifying(fileUri, success = true)
          case (VerifierResult.Failure(_), Some(c)) =>
            c.finishedGoifying(fileUri, success = false)
          case _ =>
        }
      
      case Failure(_) =>
        VerifierState.client match {
          case Some(c) => c.finishedGoifying(fileUri, success = false)
          case None =>
        }
    }

    goifyFuture
  }


  /**
    * Gobrafy File.
    */
  def gobrafy(fileData: FileData): Unit = {
    var success = false

    val filePath = fileData.filePath
    //val fileUri = fileData.fileUri

    val newFilePath = Helper.gobraFileExtension(filePath)
    val newFileUri = Helper.gobraFileExtension(fileData.fileUri)

    VerifierState.removeDiagnostics(newFileUri)
    VerifierState.removeVerificationInformation(newFileUri)

    if (newFileUri == VerifierState.openFileUri) VerifierState.publishDiagnostics(newFileUri)

    try {
      val fileBuffer = Source.fromFile(filePath)
      val fileContents = fileBuffer.mkString
      fileBuffer.close()
      
      val gobraFile = new File(newFilePath)
      val bw = new BufferedWriter(new FileWriter(gobraFile))

      bw.write(Gobrafier.gobrafyFileContents(fileContents))
      bw.close()

      success = true  
    } catch {
      case _: Throwable => // just fall through case since we were pessimistic with the success.
    }

    VerifierState.client match {
        case Some(c) => c.finishedGobrafying(filePath, newFilePath, success)
        case None =>
    }
  }

  /**
    * Get preview of Code which then gets displayed on the client side.
    * Currently the internal representation and the viper encoding can be previewed.
    */
  def codePreview(fileData: FileData, internalPreview: Boolean, viperPreview: Boolean, selections: List[Range])(executor: GobraExecutionContext): Future[VerifierResult] = {
    val config = Helper.previewConfigFromTask(fileData, internalPreview, viperPreview, selections)
    verifier.verify(config)(executor)
  }


  def stop(): Future[Unit] = {
    _server.stop().map(_ => ())(_executor)
  }

  def flushCache(): Unit = {
    _server.flushCache()
    VerifierState.flushCachedDiagnostics()
    VerifierState.changes = List()
  }

  def delete(): Unit = {
    ViperServerBackend.resetServer()
    ViperServerBackend.resetExecutor()
    _server = null
  } 
}
