package routing

object RoutingConversion {

  type Fun0 = HttpRequest => HttpResponse
  type Fun1 = (HttpRequest, String) => HttpResponse

  given Conversion[Fun0, HttpHandler[0]] with
    def apply(fun: Fun0) = args => fun(args._1)

  given Conversion[Fun1, HttpHandler[1]] with
    def apply(fun: Fun1): HttpHandler[1] = args => fun(args._1, args._2.get(0))

}
