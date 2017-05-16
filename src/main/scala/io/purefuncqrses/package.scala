package io

import scala.language.higherKinds

package object purefuncqrses {

  case class Identity[+A](value: A)

//  type State2[S1, S2, M[+ _], +A] = S2 => StateTransformed[S1, M, A]
//
//  type State3[S1, S2, S3, M[+ _], A] = S3 => State2[S2, S1, M, (S1, A)]

}
