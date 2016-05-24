package com.example

import japgolly.scalajs.react._
import org.scalajs.dom.raw.{File, FileList}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom.document
//import org.scalajs.jquery.jQuery
import japgolly.scalajs.react.vdom.prefix_<^._

object Hello extends JSApp {

  case class State(dragOver: Boolean, fileList: Option[FileList])
  class Backend($: BackendScope[String, State]) {
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
      $.setState(State(false, Some(e.dataTransfer.files))) >>
        Callback.log(e.dataTransfer.files)
    }

    def render(p: String, s: State) = {
      <.div(^.classSet("main" -> true),
        <.div(^.classSet("col"->true), DropArea((s, this))),
        <.div(^.classSet("col"->true), AudioFileList((s, this))),
        <.div(^.classSet("col"->true))
      )
    }
  }


  val TakoraApp = ReactComponentB[String]("App")
    .initialState[State](State(false, None))
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


  val ScoreText = ReactComponentB[String]("ScoreText")
    .render_P(scoreText => <.span(^.className := "alignleft", "Score: " + scoreText))
    .build

  @JSExport
  override def main(): Unit = {
    ReactDOM.render(<.div(TakoraApp("Main")), document.getElementById("main"))
  }

}
