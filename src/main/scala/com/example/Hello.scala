package com.example

import java.net.URL

import japgolly.scalajs.react._
import org.scalajs.dom.raw.{FileReader, File, FileList}
import org.scalajs.dom.raw.UIEvent

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom.document
import japgolly.scalajs.react.vdom.prefix_<^._

object Hello extends JSApp {

  case class State(dragOver: Boolean, fileList: Option[FileList], selectedFile: Option[Int], fileUrl: Option[String])
  class Backend($: BackendScope[String, State]) {
//    def onFileLoad(e: ReactEventAliases#Read)

    def onDragOver(e: ReactEventAliases#ReactDragEventH): Callback = {
      e.stopPropagation()
      e.preventDefault()
      e.dataTransfer.dropEffect = "copy"
      $.modState(_.copy(dragOver = true))
    }

    def onDragLeave(e: ReactEventAliases#ReactEventH): Callback = {
      $.modState(_.copy(dragOver = false))
    }


    def onDrop(e: ReactEventAliases#ReactDragEventH): Callback = {
      e.stopPropagation()
      e.preventDefault()
      e.dataTransfer.files
      val fileReader = new FileReader
      val file = e.dataTransfer.files.item(0)
      fileReader.readAsDataURL(file)
      fileReader.onload = (e: UIEvent) => {
        // Cast is OK, since we are calling readAsText
        val contents = fileReader.result.asInstanceOf[String]
        println("the shit is loaded")
        $.modState(s => s.copy(fileUrl = Some(contents))).runNow()
      }

//      Callback.
      $.setState(State(false, Some(e.dataTransfer.files), Some(0), None)) >>
        Callback.log(e.dataTransfer.files)
    }

    def render(p: String, s: State) = {
      <.div(^.classSet("main" -> true),
        <.div(^.classSet("col"->true), DropArea((s, this))),
        <.div(^.classSet("col"->true), AudioFileList((s, this))),
        <.div(^.classSet("col"->true), if (s.fileUrl.isEmpty) "stuff" else AudioPlayer(s.fileUrl.get))
      )
    }
  }


  val TakoraApp = ReactComponentB[String]("App")
    .initialState[State](State(false, None, None, None))
    .renderBackend[Backend].build

  val DropArea = ReactComponentB[(State, Backend)]("DropArea")
      .render_P({ case (s, b) =>
        <.div(
          ^.id := "drop_zone",
          ^.onDragOver ==> b.onDragOver,
          ^.onDragLeave ==> b.onDragLeave,
          ^.onDrop ==> b.onDrop,
          ^.classSet("over" -> s.dragOver),
          (if (s.dragOver) "Release audio file" else "Drop audio file here") + " btw my name is " + s.dragOver
        )
      }).build

  val AudioFileElement = ReactComponentB[File]("AudioFileElement")
    .render_P(p => <.option(^.classSet("active" -> p.name.endsWith(".mp3")), p.name))
    .build

  val AudioFileList = ReactComponentB[(State, Backend)]("AudioFileList")
      .render_P({
        case (s, b) =>
          s.fileList.fold(
            <.div("EMPTY")
          )({
            fileList =>
              val ab = ArrayBuffer[TagMod]()
              for {
                i <- 0 until fileList.length
              } {
                ab += AudioFileElement(fileList.item(i))
              }
//              ab += (^.size := fileList.length)
              <.div(
                <.select(ab.toSeq:_*)
              )
          })}
      ).build


  class AudioplayerBackend($: BackendScope[String, (Option[Float], Option[Float])]) {

  }

//  val Example = ReactComponentB[Unit]("Example")
//    .initialState(Vector("hello", "world"))
//    .backend(new Backend(_))
//    .render(_.backend.render)
//    .build

  val AudioPlayer = ReactComponentB[String]("AudioPlayer")

      .initialState[(Option[Float], Option[Float])](None, None)
    .backend(new AudioplayerBackend(_))
    .renderPS({
      case (_, p, s) =>
        <.audio(^.autoPlay := true, ^.controls := true, <.source(^.src := p), "Your browser does not support the audio element.")
    }).build

  @JSExport
  override def main(): Unit = {
    ReactDOM.render(<.div(TakoraApp("Main")), document.getElementById("main"))
  }

}
