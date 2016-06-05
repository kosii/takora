package com.example

import java.net.URL

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.TimerSupport
import org.scalajs.dom.html.Audio
import org.scalajs.dom.raw.{FileReader, File, FileList}
import org.scalajs.dom.raw.UIEvent

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom.document
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

import scala.concurrent.duration._

object Hello extends JSApp {

  val AudioRef = Ref("grrrr")
  val OuterRef = Ref("o")

  case class State(
                    dragOver: Boolean,
                    fileList: Option[FileList],
                    selectedFile: Option[Int],
                    fileUrl: Option[String],
                    start: Option[Float],
                    end: Option[Float])
  class Backend($: BackendScope[String, State]) extends TimerSupport {
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


    def onKeydown(e: ReactKeyboardEvent): Callback = {
      def stuff = CallbackOption.keyCodeSwitch(e, ctrlKey = false) {
        case KeyCode.R =>
          Callback.log("RRRRRRR")
      }

      stuff >> e.preventDefaultCB


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
        $.modState(s => s.copy(fileUrl = Some(contents))).runNow()
      }

//      Callback.
      $.setState(State(false, Some(e.dataTransfer.files), Some(0), None, None, None)) >>
        Callback.log(e.dataTransfer.files)
    }




    def render(p: String, s: State) = {
      <.div(
        ^.ref := OuterRef,
        ^.classSet("main" -> true),
        ^.onKeyDown ==> onKeydown,
        ^.tabIndex := 0,
        <.div(^.classSet("col"->true), DropArea((s, this))),
        <.div(^.classSet("col"->true), AudioFileList((s, this))),
        <.div(^.classSet("col"->true), if (s.fileUrl.isEmpty) "stuff" else AudioPlayer((s.fileUrl.get, this)))
      )
    }

    def init: Callback = {
      Callback.log(OuterRef($).isDefined) >>
      OuterRef($).tryFocus
    }

    def componentWillMount() = {
      document.onkeydown
    }
  }


  val TakoraApp = ReactComponentB[String]("App")
    .initialState[State](State(false, None, None, None, None, None))
    .renderBackend[Backend]
    .componentDidMount({ c =>
      c.backend.init >> c.backend.setInterval(Callback.log(AudioRef(c).toString), 500 millisecond)
    })
    .build

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
              <.div(
                <.select(ab.toSeq:_*)
              )
          })}
      ).build



  val AudioPlayer = ReactComponentB[(String, Backend)]("AudioPlayer")
//      .initialState[(Option[Float], Option[Float])](None, None)
    .render({ $ =>
        <.audio(
          ^.autoPlay  := true,
          ^.ref       := AudioRef,
          ^.controls  := true,
          <.source(^.src := $.props._1),
          "Your browser does not support the audio element."
        )
    }).build

  @JSExport
  override def main(): Unit = {
    ReactDOM.render(<.div(TakoraApp("Main")), document.getElementById("main"))
  }

}
