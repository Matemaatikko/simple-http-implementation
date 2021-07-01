package routing


enum HttpMethod:
  case GET
  case POST
  case PUT
  case DELETE
  case HEAD
  case OPTIONS
  case TRACE

enum Path:
  case Url(value: String)
  case Route(value: String)

enum HttpVersion:
  case `Http/1.0`
  case `Http/1.1`

//See: https://en.wikipedia.org/wiki/List_of_HTTP_status_codes

enum StatusCode(number: Int, message: String):
  case Continue extends StatusCode(100, "Continue")
  case SwitchingProtocols extends StatusCode(101, "Switching Protocols")
  case Processing extends StatusCode(102, "Processing ")
  case EarlyHints extends StatusCode(103, "Early Hints")

  case OK extends StatusCode(200, "OK")
  case Created extends StatusCode(201, "Created")
  case Accepted extends StatusCode(202, "Accepted")
  case NonAuthoritativeInformation extends StatusCode(203, "Non-Authoritative Information")
  case NoContent extends StatusCode(204, "No Content")
  case ResetContent extends StatusCode(205, "Reset Content")
  case PartialContent extends StatusCode(206, "Partial Content")
  case MultiStatus extends StatusCode(207, "Multi-Status")
  case AlreadyReported extends StatusCode(208, "Already Reported")
  case IMUsed extends StatusCode(226, "IM Used")

  case MultipleChoices extends StatusCode(300, "Multiple Choices")
  case MovedPermanently extends StatusCode(301, "Moved Permanently")
  case Found extends StatusCode(302, "Found")
  case SeeOther extends StatusCode(303, "See Other")
  case NotModified extends StatusCode(304, "Not Modified")
  case UseProxy extends StatusCode(305, "Use Proxy")
  case SwitchProxy extends StatusCode(306, "Switch Proxy")
  case TemporaryRedirect extends StatusCode(307, "Temporary Redirect")
  case PermanentRedirect extends StatusCode(308, "Permanent Redirect")

//TODO Add rest of status codes



case class HttpRequest(
                        httpMethod: HttpMethod,
                        path: Path,
                        version: HttpVersion,
                        httpHeaders: Map[String, String],
                        body: Option[String]
                      )

case class HttpResponse(
                         version: HttpVersion,
                         statusCode: StatusCode,
                         headers: Map[String, String],
                         body: Option[String]
                       )