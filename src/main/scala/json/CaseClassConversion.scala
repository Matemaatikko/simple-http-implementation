package json


object CaseClassConversion {

  def jsonConversion1[A0, A](constructor: A0 => A)(using cls: Class[A],
                            a0: JsonConversion[A0]
                           ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion2[A0, A1, A](constructor: (A0, A1) => A)(using cls: Class[A],
                                a0: JsonConversion[A0], a1: JsonConversion[A1]
                               ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion3[A0, A1, A2, A]
  (constructor: (A0, A1, A2) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion4[A0, A1, A2, A3, A]
  (constructor: (A0, A1, A2, A3) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion5[A0, A1, A2, A3, A4, A]
  (constructor: (A0, A1, A2, A3, A4) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion6[A0, A1, A2, A3, A4, A5, A]
  (constructor: (A0, A1, A2, A3, A4, A5) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion7[A0, A1, A2, A3, A4, A5, A6, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion8[A0, A1, A2, A3, A4, A5, A6, A7, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion9[A0, A1, A2, A3, A4, A5, A6, A7, A8, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }


  def jsonConversion12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion15[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion16[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion17[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15],
   a16: JsonConversion[A16]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15]),
        fields(16).getName -> a16.toJson(fields(16).get(a).asInstanceOf[A16])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2),
          a16.fromJson(paramList.find( _._1 == orderedParams(16)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion18[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15],
   a16: JsonConversion[A16], a17: JsonConversion[A17]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15]),
        fields(16).getName -> a16.toJson(fields(16).get(a).asInstanceOf[A16]),
        fields(17).getName -> a17.toJson(fields(17).get(a).asInstanceOf[A17])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2),
          a16.fromJson(paramList.find( _._1 == orderedParams(16)).get._2),
          a17.fromJson(paramList.find( _._1 == orderedParams(17)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }

  def jsonConversion19[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15],
   a16: JsonConversion[A16], a17: JsonConversion[A17], a18: JsonConversion[A18]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15]),
        fields(16).getName -> a16.toJson(fields(16).get(a).asInstanceOf[A16]),
        fields(17).getName -> a17.toJson(fields(17).get(a).asInstanceOf[A17]),
        fields(18).getName -> a18.toJson(fields(18).get(a).asInstanceOf[A18])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2),
          a16.fromJson(paramList.find( _._1 == orderedParams(16)).get._2),
          a17.fromJson(paramList.find( _._1 == orderedParams(17)).get._2),
          a18.fromJson(paramList.find( _._1 == orderedParams(18)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }


  def jsonConversion20[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15],
   a16: JsonConversion[A16], a17: JsonConversion[A17], a18: JsonConversion[A18], a19: JsonConversion[A19]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15]),
        fields(16).getName -> a16.toJson(fields(16).get(a).asInstanceOf[A16]),
        fields(17).getName -> a17.toJson(fields(17).get(a).asInstanceOf[A17]),
        fields(18).getName -> a18.toJson(fields(18).get(a).asInstanceOf[A18]),
        fields(19).getName -> a19.toJson(fields(19).get(a).asInstanceOf[A19])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2),
          a16.fromJson(paramList.find( _._1 == orderedParams(16)).get._2),
          a17.fromJson(paramList.find( _._1 == orderedParams(17)).get._2),
          a18.fromJson(paramList.find( _._1 == orderedParams(18)).get._2),
          a19.fromJson(paramList.find( _._1 == orderedParams(19)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }


  def jsonConversion21[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15],
   a16: JsonConversion[A16], a17: JsonConversion[A17], a18: JsonConversion[A18], a19: JsonConversion[A19],
   a20: JsonConversion[A20]
  ): JsonConversion[A] = new JsonConversion[A] {
    def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15]),
        fields(16).getName -> a16.toJson(fields(16).get(a).asInstanceOf[A16]),
        fields(17).getName -> a17.toJson(fields(17).get(a).asInstanceOf[A17]),
        fields(18).getName -> a18.toJson(fields(18).get(a).asInstanceOf[A18]),
        fields(19).getName -> a19.toJson(fields(19).get(a).asInstanceOf[A19]),
        fields(20).getName -> a20.toJson(fields(20).get(a).asInstanceOf[A20])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2),
          a16.fromJson(paramList.find( _._1 == orderedParams(16)).get._2),
          a17.fromJson(paramList.find( _._1 == orderedParams(17)).get._2),
          a18.fromJson(paramList.find( _._1 == orderedParams(18)).get._2),
          a19.fromJson(paramList.find( _._1 == orderedParams(19)).get._2),
          a20.fromJson(paramList.find( _._1 == orderedParams(20)).get._2)
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }


  def jsonConversion22[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A]
  (constructor: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => A)
  (using cls: Class[A],
   a0: JsonConversion[A0], a1: JsonConversion[A1], a2: JsonConversion[A2], a3: JsonConversion[A3],
   a4: JsonConversion[A4], a5: JsonConversion[A5], a6: JsonConversion[A6], a7: JsonConversion[A7],
   a8: JsonConversion[A8], a9: JsonConversion[A9], a10: JsonConversion[A10], a11: JsonConversion[A11],
   a12: JsonConversion[A12], a13: JsonConversion[A13], a14: JsonConversion[A14], a15: JsonConversion[A15],
   a16: JsonConversion[A16], a17: JsonConversion[A17], a18: JsonConversion[A18], a19: JsonConversion[A19],
   a20: JsonConversion[A20], a21: JsonConversion[A21]
  ): JsonConversion[A] = new JsonConversion[A] {
   def toJson(a: A) =
      val fields = cls.getDeclaredFields
      fields.map(_.setAccessible(true))
      JsObject(Seq(
        fields(0).getName -> a0.toJson(fields(0).get(a).asInstanceOf[A0]),
        fields(1).getName -> a1.toJson(fields(1).get(a).asInstanceOf[A1]),
        fields(2).getName -> a2.toJson(fields(2).get(a).asInstanceOf[A2]),
        fields(3).getName -> a3.toJson(fields(3).get(a).asInstanceOf[A3]),
        fields(4).getName -> a4.toJson(fields(4).get(a).asInstanceOf[A4]),
        fields(5).getName -> a5.toJson(fields(5).get(a).asInstanceOf[A5]),
        fields(6).getName -> a6.toJson(fields(6).get(a).asInstanceOf[A6]),
        fields(7).getName -> a7.toJson(fields(7).get(a).asInstanceOf[A7]),
        fields(8).getName -> a8.toJson(fields(8).get(a).asInstanceOf[A8]),
        fields(9).getName -> a9.toJson(fields(9).get(a).asInstanceOf[A9]),
        fields(10).getName -> a10.toJson(fields(10).get(a).asInstanceOf[A10]),
        fields(11).getName -> a11.toJson(fields(11).get(a).asInstanceOf[A11]),
        fields(12).getName -> a12.toJson(fields(12).get(a).asInstanceOf[A12]),
        fields(13).getName -> a13.toJson(fields(13).get(a).asInstanceOf[A13]),
        fields(14).getName -> a14.toJson(fields(14).get(a).asInstanceOf[A14]),
        fields(15).getName -> a15.toJson(fields(15).get(a).asInstanceOf[A15]),
        fields(16).getName -> a16.toJson(fields(16).get(a).asInstanceOf[A16]),
        fields(17).getName -> a17.toJson(fields(17).get(a).asInstanceOf[A17]),
        fields(18).getName -> a18.toJson(fields(18).get(a).asInstanceOf[A18]),
        fields(19).getName -> a19.toJson(fields(19).get(a).asInstanceOf[A19]),
        fields(20).getName -> a20.toJson(fields(20).get(a).asInstanceOf[A20]),
        fields(21).getName -> a21.toJson(fields(21).get(a).asInstanceOf[A21])
      ))

    def fromJson(jsValue: JsValue) = jsValue match {
      case JsObject(paramList) =>
        val orderedParams = cls.getDeclaredFields.map(_.getName)
        constructor(
          a0.fromJson(paramList.find( _._1 == orderedParams(0)).get._2),
          a1.fromJson(paramList.find( _._1 == orderedParams(1)).get._2),
          a2.fromJson(paramList.find( _._1 == orderedParams(2)).get._2),
          a3.fromJson(paramList.find( _._1 == orderedParams(3)).get._2),
          a4.fromJson(paramList.find( _._1 == orderedParams(4)).get._2),
          a5.fromJson(paramList.find( _._1 == orderedParams(5)).get._2),
          a6.fromJson(paramList.find( _._1 == orderedParams(6)).get._2),
          a7.fromJson(paramList.find( _._1 == orderedParams(7)).get._2),
          a8.fromJson(paramList.find( _._1 == orderedParams(8)).get._2),
          a9.fromJson(paramList.find( _._1 == orderedParams(9)).get._2),
          a10.fromJson(paramList.find( _._1 == orderedParams(10)).get._2),
          a11.fromJson(paramList.find( _._1 == orderedParams(11)).get._2),
          a12.fromJson(paramList.find( _._1 == orderedParams(12)).get._2),
          a13.fromJson(paramList.find( _._1 == orderedParams(13)).get._2),
          a14.fromJson(paramList.find( _._1 == orderedParams(14)).get._2),
          a15.fromJson(paramList.find( _._1 == orderedParams(15)).get._2),
          a16.fromJson(paramList.find( _._1 == orderedParams(16)).get._2),
          a17.fromJson(paramList.find( _._1 == orderedParams(17)).get._2),
          a18.fromJson(paramList.find( _._1 == orderedParams(18)).get._2),
          a19.fromJson(paramList.find( _._1 == orderedParams(19)).get._2),
          a20.fromJson(paramList.find( _._1 == orderedParams(20)).get._2),
          a21.fromJson(paramList.find( _._1 == orderedParams(21)).get._2),
        )
      case _ => error(jsValue, cls.getName.toString)
    }
  }


}
