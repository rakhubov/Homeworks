import scala.math.Ordered.orderingToOrdered

object error_handling {
  case class PaymentCard(
                          name: String,
                          number: String,
                          expirationDate: String,
                          securityCode: String,
                          bankName: String,
                          paymantSystem: String
                        ) {
    val youName = name
    val nomberCard = number
    val ExpirationDateCard = expirationDate
    val securityCodeCard = securityCode
    val bankNameCard = bankName
    val paymantSystemCard = paymantSystem
  }

  sealed trait ValidationError
  object ValidationError {
    final case object NameCardLength extends ValidationError {
      override def toString = "Name card must be between 6 to 30 charters"
    }
    final case object NomberUseFigure extends ValidationError {
      override def toString = "Nomber card can not have letters"
    }
    final case object NomberCardLength extends ValidationError {
      override def toString = "Nomber card must be 16 digits"
    }
    final case object DateTwoNombers extends ValidationError {
      override def toString = "Date card must be 2 nomber"
    }
    final case object DateUseNombers extends ValidationError {
      override def toString = "Date card can not have letters"
    }
    final case object DateYearsMountsValide extends ValidationError {
      override def toString =
        "Years must be between 16 to 21, Mounts must be between 1 to 12"
    }
    final case object SecurityCodeUseNombers extends ValidationError {
      override def toString = "Security Code card can not have letters"
    }
    final case object SecurityCode4Nombers extends ValidationError {
      override def toString = "Security Code card must have 4 digits"
    }
    final case object BankNameValide extends ValidationError {
      override def toString =
        "Bank name must be BelarusBank or BPSBank or MTBBank"
    }
    final case object PaymentSystemValide extends ValidationError {
      override def toString = "Paymant system must be Viza or MasterCard"
    }
  }

  import cats.data.ValidatedNec
  import cats.syntax.all._
  import ValidationError._


  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validateNameCardLength(youName: String): AllErrorsOr[String] = {
      if (youName.length >= 6 && youName.length <= 30) youName.validNec
      else NameCardLength.invalidNec
    }


    def validateNomberCard(nomber: String): AllErrorsOr[String]= {
      def validateNomberUseFigure: AllErrorsOr[String]=
        nomber.toIntOption match {
          case Some(value) => nomber.validNec
          case _ => NomberUseFigure.invalidNec
        }
      def validateNomberCardLength:AllErrorsOr[String] = {
        if(BigDecimal(nomber).scale == 16) nomber.validNec
        else NomberCardLength.invalidNec
      }
      validateNomberUseFigure *> validateNomberCardLength
    }


    def valideteDateCard(date: String): AllErrorsOr[String]={
      def validateDateTwoNombers:AllErrorsOr[String]= {
        if(date.split("\\s+").toList.length == 2) date.validNec
        else DateTwoNombers.invalidNec
      }
      def valideteDateUseNombers:AllErrorsOr[String] =
        date.toIntOption match {
          case Some(value) => date.validNec
          case _ => DateUseNombers.invalidNec
        }
      def validateDateYearsMounts:AllErrorsOr[String]=
        date.split("\\s+").toList match {
          case a :: b :: Nil if (a.toIntOption >= Option(1) && a.toIntOption <= Option(12)
            && b.toIntOption >= Option(16) && b.toIntOption <= Option(21)) => date.validNec
          case _ => DateYearsMountsValide.invalidNec
        }
      validateDateTwoNombers *> valideteDateUseNombers *> validateDateYearsMounts
    }


    def validateSecurityCode(code: String): AllErrorsOr[String] ={
      def validateSecurityCodeUseNombers: AllErrorsOr[String] =
        code.toIntOption match {
          case Some(value) => code.validNec
          case _ => DateUseNombers.invalidNec
        }
      def validateSecurityCode4Nombers: AllErrorsOr[String] =
        if (BigDecimal(code).scale == 4 && code.length == 4) code.validNec
        else SecurityCode4Nombers.invalidNec
      validateSecurityCodeUseNombers *> validateSecurityCode4Nombers
    }


    def validateBankName(bankName: String): AllErrorsOr[String] =
      if(bankName == "BelarusBank" || bankName == "BPSBank" || bankName == "MTBBank") bankName.validNec
      else BankNameValide.invalidNec


    def validatePaymentSystem(paymentSystem: String): AllErrorsOr[String] =
      if(paymentSystem =="Viza" || paymentSystem == "MasterCard") paymentSystem.validNec
      else PaymentSystemValide.invalidNec




    def validate(
                  name: String, number: String, expirationDate: String, securityCode: String,
                  bankName: String, paymantSystem: String): AllErrorsOr[PaymentCard] =
      (validateNameCardLength(name), validateNomberCard(number), valideteDateCard(expirationDate),
        validateSecurityCode(securityCode), validateBankName(bankName), validatePaymentSystem(paymantSystem)).mapN(PaymentCard)
  }
}


