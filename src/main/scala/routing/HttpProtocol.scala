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

  case BadRequest extends StatusCode(400, "Bad Request")
  case Unauthorized extends StatusCode(401, "Unauthorized")
  case PaymentRequired extends StatusCode(402, "Payment Required")
  case Forbidden extends StatusCode(403, "Forbidden")
  case NotFound extends StatusCode(404, "Not Found")
  case MethodNotAllowed extends StatusCode(405, "Method Not Allowed")
  case NotAcceptable extends StatusCode(406, "Not Acceptable")
  case ProxyAuthenticationRequired extends StatusCode(407, "Proxy Authentication Required")
  case RequestTimeout extends StatusCode(408, "Request Timeout")
  case Conflict extends StatusCode(409, "Conflict")
  case Gone extends StatusCode(410, "Gone")
  case LengthRequired extends StatusCode(411, "Length Required")
  case PreconditionFailed extends StatusCode(412, "Precondition Failed")
  case PayloadTooLarge extends StatusCode(413, "Payload Too Large")
  case URITooLong extends StatusCode(414, "URI Too Long")
  case UnsupportedMediaType extends StatusCode(415, "Unsupported Media Type")
  case RangeNotSatisfiable extends StatusCode(416, "Range Not Satisfiable")
  case ExpectationFailed extends StatusCode(417, "Expectation Failed")
  case IAMATeapot extends StatusCode(418, "I'm a teapot")
  case MisdirectedRequest extends StatusCode(421, "Misdirected Request")
  case UnprocessableEntity extends StatusCode(422, "Unprocessable Entity")
  case Locked extends StatusCode(423, "Locked")
  case FailedDependency extends StatusCode(424, "Failed Dependency")
  case TooEarly extends StatusCode(425, "Too Early")
  case UpgradeRequired extends StatusCode(426, "Upgrade Required")
  case PreconditionRequired extends StatusCode(428, "Precondition Required")
  case TooManyRequests extends StatusCode(429, "Too Many Requests")
  case RequestHeaderFieldsTooLarge extends StatusCode(431, "Request Header Fields Too Large")
  case UnavailableForLegalReasons extends StatusCode(451, "Unavailable For Legal Reasons")

  case InternalServerError extends StatusCode(500, "Internal Server Error")
  case NotImplemented extends StatusCode(501, "Not Implemented")
  case BadGateway extends StatusCode(502, "Bad Gateway")
  case ServiceUnavailable extends StatusCode(503, "Service Unavailable")
  case GatewayTimeout extends StatusCode(504, "Gateway Timeout")
  case HTTPVersionNotSupported extends StatusCode(505, "HTTP Version Not Supported")
  case VariantAlsoNegotiates extends StatusCode(506, "Variant Also Negotiates")
  case InsufficientStorage extends StatusCode(507, "Insufficient Storage")
  case LoopDetected extends StatusCode(508, "Loop Detected")
  case NotExtended extends StatusCode(510, "Not Extended")
  case NetworkAuthenticationRequired extends StatusCode(511, "Network Authentication Required")
end StatusCode


case class Header(name: String, value: String)

case class HttpRequest(
                        httpMethod: HttpMethod,
                        path: Path,
                        version: HttpVersion,
                        httpHeaders: Seq[Header],
                        body: Option[String]
                      )

case class HttpResponse(
                         version: HttpVersion,
                         statusCode: StatusCode,
                         headers: Map[String, String],
                         body: Option[String]
                       )

extension (response: HttpResponse)
  def httpString: String = ???