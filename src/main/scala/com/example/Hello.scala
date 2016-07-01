package com.example

import java.net.URL

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.TimerSupport
import org.scalajs.dom.html.Audio
import org.scalajs.dom.raw.{FileReader, File, FileList}
import org.scalajs.dom.raw.UIEvent

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Promise
import scala.scalajs.js.{JSApp}
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom.document
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode

import scala.concurrent.duration._

object Hello extends JSApp {


  val OuterRef = Ref("o")

  case class State(
                    dragOver: Boolean,
                    fileList: Option[FileList],
                    selectedFile: Option[Int],
                    fileUrl: Option[String],
                    start: Double,
                    end: Option[Double]) {
    def hitRepeat(current: Double): State = {
      if (end.isDefined) {
        this.copy(start = this.end.get, end = None)
      } else {
        this.copy(end = Some(current))
      }
    }
  }
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
          AudioRef($).fold(Callback.log("RRRR"))( a =>
            Callback.log("pina") >> $.modState(_.hitRepeat(a.currentTime)))
        case KeyCode.A =>
          $.modState(s => s.copy(start = (s.start - 0.1) max 0.0))
        case KeyCode.A =>
          $.modState(s => s.copy(start = (s.start + 0.1) max 0.0))
        case KeyCode.D =>
          AudioRef($).fold(Callback.log("undef"))( a =>
            $.modState(s => s.copy(end = s.end.map(end => (end - 0.1) min a.duration))))
        case KeyCode.F =>
          AudioRef($).fold(Callback.log("undef"))( a =>
            $.modState(s => s.copy(end = s.end.map(end => (end + 0.1) min a.duration))))
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
      $.modState(_.copy(false, Some(e.dataTransfer.files), Some(0)))
      $.setState(State(false, Some(e.dataTransfer.files), Some(0), None, 0.0, None)) >>
        Callback.log(e.dataTransfer.files) >> init
    }

    def render(p: String, s: State) = {
      <.div(
        ^.ref := OuterRef,
        ^.classSet("main" -> true),
        ^.onKeyDown ==> onKeydown,
        ^.tabIndex := 0,
        <.div(^.classSet("col"->true),
          DropArea((s, this)),
          Status((s.start, s.end))
        ),
        <.div(^.classSet("col"->true), AudioFileList((s, this))),
        <.div(^.classSet("col"->true), if (s.fileUrl.isEmpty) "stuff" else AudioPlayer(s.fileUrl.get))
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

  val Status = ReactComponentB[(Double, Option[Double])]("Status")
    .render_P({ case (start, endOpt) =>
      endOpt.fold(<.div(<.span("fuck")))(end =>
        <.div(
          <.span(s"repeating between ${start} and ${end}")
        ))
    })
    .build

  val TakoraApp = ReactComponentB[String]("App")
    .initialState[State](State(false, None, None, None, 0.0, None))
    .renderBackend[Backend]
    .componentDidMount({ c =>
      c.backend.init >> c.backend.setInterval(Callback({
        if (AudioRef(c).isDefined) {
          val audio = AudioRef(c).get
          c.state.end.map({ end =>
            if (end < audio.currentTime) audio.currentTime = c.state.start
          })
          if (audio.currentTime < c.state.start) audio.currentTime = c.state.start
        }
        }), 33 millisecond
      )
    })
//    .componentDidUpdate(c => Callback.log(s"Component updated ${AudioRef(c.$)}"))
    .build

  val DropArea = ReactComponentB[(State, Backend)]("DropArea")
      .render_P({ case (s, b) =>
        <.div(
          ^.id := "drop_zone",
          ^.onDragOver ==> b.onDragOver,
          ^.onDragLeave ==> b.onDragLeave,
          ^.onDrop ==> b.onDrop,
          ^.classSet("over" -> s.dragOver),
          (if (s.dragOver) "Release audio file" else "Drop audio file here")
        )
      }).build

  def AudioPlayer(url: String) = <.audio(
    ^.ref := AudioRef,
    ^.autoPlay  := true,
    ^.controls  := true,
    <.source(^.src := url),
    "Your browser does not support the audio element."
  )


  val AudioRef = Ref[Audio]("audio")

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



  @JSExport
  override def main(): Unit = {
    ReactDOM.render(<.div(TakoraApp("Main")), document.getElementById("main"))
  }

}
