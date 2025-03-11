// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.gobraserver

import java.util.concurrent.{CompletableFuture, CompletionException}
import com.google.gson.Gson
import org.eclipse.lsp4j.{Position => LocPosition}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}
import org.eclipse.lsp4j.{DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
  DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
  InitializeParams, InitializeResult, MessageParams, MessageType, Range,
  ServerCapabilities, TextDocumentSyncKind}
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.{Location, TextDocumentPositionParams, DefinitionParams, ReferenceParams,
  CompletionOptions, CompletionItem, CompletionParams, TextDocumentContentChangeEvent,
  RenameParams, WorkspaceEdit, TextEdit, RenameOptions, MarkupContent, Hover}

import viper.gobra.ast.ScopeTree
import viper.gobra.ast.LiveTextBuffer
import org.eclipse.lsp4j.services.LanguageClient
import java.net.URI
import java.nio.file.{Files, Paths}


//check gobra.scala, parse results and type info if symbol table survives
//what does the symbol table give and what does it need, type info might know more stuff`
//understand code
// fix my intellij ---- DONE

import scala.jdk.CollectionConverters._
import scala.annotation.unused
import scala.collection.concurrent.TrieMap
import scala.collection.mutable


class GobraServerService(config: ServerConfig)(implicit executor: GobraServerExecutionContext) extends IdeLanguageClientAware {
  private val gson: Gson = new Gson()

  private var client: Option[LanguageClient] = None


  @JsonRequest(value = "initialize")
  def initialize(@unused params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    // always send full text document for each notification:
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setDeclarationProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setReferencesProvider(true)
    //capabilities.setRenameProvider(true)
    capabilities.setRenameProvider(new RenameOptions(true))
    capabilities.setHoverProvider(true)
    capabilities.setCompletionProvider(new CompletionOptions(true, List(".").asJava))

//    capabilities.setCompletionProvider(new CompletionOptions())


    val options: List[String] = List("--disablePlugins", "--logLevel", config.logLevel.levelStr)
    GobraServer.init(options)(executor)
    GobraServer.start()

    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  @JsonRequest("textDocument/hover")
  def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] = {
    CompletableFuture.supplyAsync(() => {
      try{
        val uri = params.getTextDocument.getUri
        val position = params.getPosition

        // Find symbol at cursor position
        val symbolOpt = ScopeTree.findSymbolAtPosition(uri, position)
        val symbolTyp = ScopeTree.getSymbolType(uri, symbolOpt.getOrElse(null))
        (symbolOpt, symbolTyp) match {
          case (Some(symbol), typ) if typ != null =>
            val hoverContent = new MarkupContent("markdown", s"**Symbol:** `${symbol.toString}`\n\n_Type:_ `${typ}`")
            new Hover(hoverContent)
          case (Some(_), _) | (None, _) =>
            new Hover(new MarkupContent("markdown", "")) // No hover information found
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          new Hover(new MarkupContent("markdown", ""))
      }
    })
  }


  @JsonRequest("textDocument/prepareRename")
  def prepareRename(params: RenameParams): CompletableFuture[Range] = {
    println("🚀 prepareRename triggered!")

    val uri = params.getTextDocument.getUri
    val position = params.getPosition

    ScopeTree.canRenameSymbol(uri, position) match {
      case Left(errorMessage) =>
        println(s"❌ Rename not allowed: $errorMessage")
        val error = new ResponseError(ResponseErrorCode.InvalidRequest, s"Rename Error: $errorMessage", null)
        // 🛑 Return a failed future with a custom error
        //clientLogger(errorMessage)  // Send error pop-up to VSCode
        //throw new org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorException(error)
        CompletableFuture.failedFuture(new ResponseErrorException(error))
      case Right(symbol) =>
        val range = ScopeTree.getSymbolRange(symbol, uri)
        if (range == null) {
          println("❌ Error: Symbol range is null")
          return CompletableFuture.failedFuture(new RuntimeException("Rename Error: Could not determine rename range."))
        }
        println(s"✅ Rename allowed. Range: $range")
        CompletableFuture.completedFuture(range)
    }
  }


  @JsonRequest("textDocument/rename")
  def rename(params: RenameParams): CompletableFuture[WorkspaceEdit] = {
    CompletableFuture.supplyAsync(() => {
      val uri = params.getTextDocument.getUri
      val position = params.getPosition
      val newName = params.getNewName

      println(s"🔄 Rename Request Received: ${uri} at ${position} -> '${newName}'")

      // 1️⃣ Find all references to the symbol
      val locations = ScopeTree.findReferencesInProject(uri, position)

      if (locations.isEmpty) {
        println("❌ No references found, cannot rename.")
        return CompletableFuture.completedFuture(new WorkspaceEdit())
      }

      val workspaceEdit = new WorkspaceEdit()

      println(s"🔍 Found ${locations.length} occurrences to rename:")
      val changesMap = scala.collection.mutable.Map[String, java.util.List[TextEdit]]()

      locations.foreach { loc =>
        val fileUri = loc.getUri
        val range = loc.getRange

        println(s"   📌 File: ${fileUri}, Range: ${range}")

        val textEdit = new TextEdit(range, newName)

        if (changesMap.contains(fileUri)) {
          changesMap(fileUri).add(textEdit)
        } else {
          changesMap(fileUri) = new java.util.ArrayList[TextEdit]()
          changesMap(fileUri).add(textEdit)
        }
      }

      workspaceEdit.setChanges(changesMap.asJava)

      println("✅ Rename operation completed.")

      (workspaceEdit)
    })
  }


  @JsonRequest("completionItem/resolve")
  def resolveCompletionItem(item: CompletionItem): CompletableFuture[CompletionItem] = {
    CompletableFuture.supplyAsync(() => {
      // Add extra details (if needed)
      item.setDetail(s"Resolved info for ${item.getLabel}") // Example detail
      item.setDocumentation(s"This is additional documentation for ${item.getLabel}.")

      item // Return the updated item
    })
  }

  // Completion request
  @JsonRequest("textDocument/completion")
  def completion(params: CompletionParams): CompletableFuture[java.util.List[CompletionItem]] = {
    CompletableFuture.supplyAsync(() => {
      try {
        val uri = params.getTextDocument.getUri
        val position = params.getPosition

        val path = Paths.get(new URI(uri))

        // Get completions safely
        val completionItems = ScopeTree.getCompletions(uri, position, LiveTextBuffer.getFileContent(uri))
        val allowedLabels = completionItems.map(_.getLabel).toSet

        // Filter out any irrelevant items
        val filteredItems = completionItems.filter(item => allowedLabels.contains(item.getLabel))

        // Convert to Java list and return
        new java.util.ArrayList(filteredItems.asJava)

      } catch {
        case e: Exception =>
          e.printStackTrace() // Log the error for debugging
          new java.util.ArrayList[CompletionItem]() // Return empty list to fail gracefully
      }
    })
  }


  // For "Find reference" request
  @JsonRequest("textDocument/references")
  def findReferences(params: ReferenceParams): CompletableFuture[java.util.List[Location]] = {
    CompletableFuture.supplyAsync(() => {
      try {
        val uri = params.getTextDocument.getUri
        val position = params.getPosition

        val references: List[Location] = ScopeTree.findReferencesInProject(uri, position).toList

        new java.util.ArrayList(references.asJava)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          new java.util.ArrayList()
      }
    })
  }

  // For "Goto Declaration" request
  @JsonRequest("textDocument/declaration")
  def gotoDeclaration(params: TextDocumentPositionParams): CompletableFuture[java.util.List[Location]] = {

    CompletableFuture.supplyAsync(() => {
      new java.util.ArrayList[Location]()
    })
  }

  @JsonRequest("textDocument/definition")
  def gotoDefinition(params: DefinitionParams): CompletableFuture[java.util.List[Location]] = {
    val uri = params.getTextDocument.getUri
    val position = params.getPosition
    val line = position.getLine
    val character = position.getCharacter

    val loc: Location = ScopeTree.getDefinition(line + 1, character + 1, uri)
    CompletableFuture.supplyAsync(() => {
      val arrayList = new java.util.ArrayList[Location]()
      arrayList.add(loc)
      arrayList
    })

  }

  @JsonRequest(value = "shutdown")
  def shutdown(): CompletableFuture[AnyRef] = {
    GobraServer.stop()
    CompletableFuture.completedFuture(null)
  }

  @JsonNotification(value = "initialized")
  def initialized(): Unit = {}

  @JsonNotification(value = "exit")
  def exit(): Unit = {
    GobraServer.delete()
    sys.exit()
  }

  // This is received when a setting is changed.
  @JsonNotification("$/setTraceNotification")
  def setTraceNotification(@unused params: Any): Unit = {}

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    val text = params.getTextDocument.getText
    LiveTextBuffer.initializeBuffer(uri, text)
    VerifierState.openFileUri = params.getTextDocument.getUri
  }

  @JsonNotification("textDocument/didChange")
  def didChange(params: DidChangeTextDocumentParams): Unit = {
    val fileUri = params.getTextDocument.getUri
    val changes = params.getContentChanges.asScala.toList
    val content = params.getContentChanges.get(0).getText
    LiveTextBuffer.updateBuffer(fileUri, changes)
    LiveTextBuffer.updateFile(fileUri, content)

    VerifierState.updateDiagnostics(fileUri, changes)

    if (VerifierState.verificationRunning > 0) {
      VerifierState.changes = VerifierState.changes :+ (fileUri, changes)
    }
  }

  @JsonNotification("gobraServer/setOpenFileUri")
  def setOpenFileUri(fileUri: String): Unit = {
    VerifierState.openFileUri = fileUri
  }

  @JsonNotification("textDocument/didClose")
  def didClose(@unused params: DidCloseTextDocumentParams): Unit = {
    // val fileUri = params.getTextDocument.getUri
    // TODO: need to remove diagnostics and forget file in ViperServer
    // VerifierState.removeDiagnostics(fileUri)
  }

  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    LiveTextBuffer.reloadFile((uri))
  }

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(@unused params: DidChangeWatchedFilesParams): Unit = {}

  @JsonNotification("gobraServer/verify")
  def verify(configJson: String): Unit = {
    var config: VerifierConfig = gson.fromJson(configJson, classOf[VerifierConfig])
    // isolate is a newly introduced field. To be backwards compatible, we allow it to not exist in the string
    // in this case, gson will simply set the corresponding field to null which is unexpected and thus get's replaced
    // by a sensible default value:
    if (config.isolate == null) {
      config = config.copy(isolate = Array.empty)
    }
    val fileUris = config.fileData.map(_.fileUri).toVector
    VerifierState.updateVerificationInformation(fileUris, Left(0))
    GobraServer.preprocess(config)
  }


  @JsonNotification("gobraServer/goifyFile")
  def goifyFile(fileDataJson: String): Unit = {
    val fileData: FileData = gson.fromJson(fileDataJson, classOf[FileData])
    GobraServer.goify(fileData)
  }
//typeInfo 1669177980
  //typeInfo 2069878272

  @JsonNotification("gobraServer/gobrafyFile")
  def gobrafyFile(fileDataJson: String): Unit = {
    val fileData: FileData = gson.fromJson(fileDataJson, classOf[FileData])
    GobraServer.gobrafy(fileData)
  }


  @JsonNotification("gobraServer/changeFile")
  def changeFile(fileDataJson: String): Unit = {
    val fileData: FileData = gson.fromJson(fileDataJson, classOf[FileData])
    VerifierState.openFileUri = fileData.fileUri
    VerifierState.publishDiagnostics(VerifierState.openFileUri)
    VerifierState.sendVerificationInformation(VerifierState.openFileUri)
  }

  @JsonNotification("gobraServer/flushCache")
  def flushCache(): Unit = {
    GobraServer.flushCache()
    VerifierState.flushCachedDiagnostics()

    VerifierState.client match {
      case Some(c) => c.showMessage(new MessageParams(MessageType.Info, "Successfully flushed ViperServer Cache."))
      case None =>
    }
  }

  @JsonNotification("gobraServer/codePreview")
  def codePreview(previewDataJson: String): Unit = {
    val previewData: PreviewData = gson.fromJson(previewDataJson, classOf[PreviewData])
    val selections = previewData.selections.map(selection => new Range(selection(0), selection(1))).toList
    GobraServer.codePreview(previewData.fileData, previewData.internalPreview, previewData.viperPreview, selections)(executor)
  }

  def clientLogger(message: String, messageType: MessageType = MessageType.Error): Unit = {
    client.foreach(_.showMessage(new MessageParams(messageType, message)))
  }


  override def connect(client: IdeLanguageClient): Unit = {
    this.client = Some(client)  // Store the client reference
    VerifierState.setClient(client)
  }
}




