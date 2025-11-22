package org.aranadedoros
package concurrency

import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

object SecureClipboard:

  private val ec: ExecutionContext =
    ExecutionContext.global

  def copyTemporarily(content: String, duration: FiniteDuration): Unit =
    val chars = content.toCharArray
    val selection = new StringSelection(new String(chars))
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(selection, null)

    Future {
      Thread.sleep(duration.toMillis)
      clipboard.setContents(new StringSelection(""), null)
      java.util.Arrays.fill(chars, '\u0000')
    }(ec)
