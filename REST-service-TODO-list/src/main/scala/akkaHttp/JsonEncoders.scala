package akkaHttp

import TodoRegistry.ActionPerformed
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

trait JsonEncoders extends DefaultJsonProtocol {

  // unmarshalling and marshalling
  implicit val taskFormat: RootJsonFormat[Task] = jsonFormat2(Task)
  implicit val taskListFormat: RootJsonFormat[List[Task]] = DefaultJsonProtocol.listFormat
  implicit val actionPerformed: RootJsonFormat[ActionPerformed] = jsonFormat1(ActionPerformed)

}
