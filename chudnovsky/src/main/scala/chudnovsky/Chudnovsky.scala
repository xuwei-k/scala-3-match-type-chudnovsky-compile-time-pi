package chudnovsky

import compile_time.Rational
import compile_time.SInt
import compile_time.SRational
import compile_time.UInt
import java.math.MathContext

object Chudnovsky {
  type _40 = UInt.FromInt[40]
  type _100 = UInt.FromInt[100]
  type _200 = UInt.FromInt[200]
  type _426880 = UInt.FromInt[426880]
  type _640320 = UInt.FromInt[640320]
  type _13591409 = UInt.FromInt[13591409]
  type _545140134 = UInt.FromInt[545140134]

  val context = new MathContext(100000)

  val pi: String =
    "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"

  def main(args: Array[String]): Unit = {
    val s"${x1} / ${x2}" = valueOf[Rational.ToBinString[F3[UInt._3, 15]]].runtimeChecked

    val resultBigDecimal = BigDecimal(
      bitToBigInt(x1),
      context
    ) / BigDecimal(
      bitToBigInt(x2),
      context
    )
    val resultString = resultBigDecimal.toString().take(300)
    println(resultString)
    val matchDigitsCount = resultString.zip(pi).takeWhile(_ == _).size - 2
    println(s"match ${matchDigitsCount} digits")
  }

  def bitToBigInt(x: String): BigInt = {
    val bits = new java.util.BitSet()
    x.reverse.zipWithIndex.foreach {
      case ('1', index) =>
        bits.set(index, true)
      case ('0', index) =>
        bits.set(index, false)
      case (other, _) =>
        sys.error(s"unexpected ${other}")
    }
    BigInt(1, bits.toByteArray.reverse)
  }

  type Numerator[K <: UInt] =
    SInt.Mult[
      UInt.Rem[K, UInt._2] match {
        case UInt._0 =>
          SInt._1
        case UInt._1 =>
          SInt.Negate[SInt._1]
      },
      SInt.Mult[
        SInt.FromUInt[
          UInt.Factorial[UInt.Mult[UInt._6, K]]
        ],
        SInt.FromUInt[
          UInt.Add[
            _13591409,
            UInt.Mult[
              _545140134,
              K
            ]
          ]
        ]
      ]
    ]

  type Denominator[K <: UInt] =
    UInt.Mult[
      UInt.Factorial[UInt.Mult[K, UInt._3]],
      UInt.Mult[
        UInt.Exp[UInt.Factorial[K], UInt._3],
        UInt.Exp[
          _640320,
          UInt.Mult[K, UInt._3]
        ]
      ]
    ]

  type SquareRoot10005[N <: Int] =
    ContinuedFraction10005SquareRoot[N, Rational._1]

  type ContinuedFraction10005SquareRoot[N <: Int, ACC <: Rational] <: Rational =
    N match {
      case 0 =>
        ACC
      case _ =>
        ContinuedFraction10005SquareRoot[
          scala.compiletime.ops.int.-[N, 1],
          Rational.Add[
            Rational.Impl[
              N match {
                case 1 =>
                  _100
                case _ =>
                  scala.compiletime.ops.int.%[N, 2] match {
                    case 0 =>
                      _40
                    case 1 =>
                      _200
                  }
              },
              UInt._1
            ],
            Rational.Inverse[ACC]
          ]
        ]
    }

  type F1[K <: UInt] =
    SRational.Impl[
      Numerator[K],
      Denominator[K]
    ]

  type F2[K <: UInt] =
    F2Impl[K, SRational._0]

  type F2Impl[K <: UInt, ACC <: SRational] =
    K match {
      case UInt._0 =>
        SRational.Add[ACC, F1[UInt._0]]
      case _ =>
        F2Impl[
          UInt.Minus[K, UInt._1],
          SRational.Add[
            ACC,
            F1[K]
          ]
        ]
    }

  type F3[K <: UInt, N <: Int] <: Rational =
    F2[K] match {
      case SRational.Impl[x, y1] =>
        x match {
          case SInt.Impl[z1, x1] =>
            SquareRoot10005[N] match {
              case Rational.Impl[x2, y2] =>
                Rational.Impl[
                  UInt.Mult[
                    y1,
                    UInt.Mult[
                      _426880,
                      x2
                    ]
                  ],
                  UInt.Mult[
                    x1,
                    y2
                  ]
                ]
            }
        }
    }

}
