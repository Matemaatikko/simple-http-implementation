package json.conversion

object CaseClassConversionGeneration {

  def genUntil(i: Int): String =
    (1 to i).map(gen(_)).mkString("")

  def gen(i: Int): String =
    val range = (0 until i).map(_.toString)
    s"""
       |  def jsonConversion${i}[${range.map(a => "A" + a).mkString(", ")}, A]
       |    (constructor: (${range.map(a => "A" + a).mkString(", ")}) => A)
       |    (using cls: Class[A],
       |      ${range.map(a => s"a${a}: JsonConversion[A${a}]").mkString(",\n\t  ")}
       |    ): JsonConversion[A] = new JsonConversion[A] {
       |    def toJson(a: A) =
       |      val fields = cls.getDeclaredFields
       |      fields.map(_.setAccessible(true))
       |      JsObject(Seq(
       |        ${range.map(a => s"fields(${a}).getName -> a${a}.toJson(fields(${a}).get(a).asInstanceOf[A${a}])").mkString(",\n\t\t")}
       |      ))
       |
       |    def fromJson(jsValue: JsValue) = jsValue match {
       |      case JsObject(paramList) =>
       |        val orderedParams = cls.getDeclaredFields.map(_.getName)
       |        constructor(
       |          ${range.map(a => s"a${a}.fromJson(paramList.find( _._1 == orderedParams(${a})).get._2)").mkString(",\n\t\t  ")}
       |        )
       |      case _ => error(jsValue, cls.getName.toString)
       |    }
       |  }
       |""".stripMargin
}
