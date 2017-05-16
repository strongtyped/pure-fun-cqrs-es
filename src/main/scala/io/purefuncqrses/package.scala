package io

import scala.language.higherKinds

package object purefuncqrses {

  case class Identity[+A](value: A)

}
