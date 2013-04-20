package mpidns.data

import scala.io.Source

object ZoneFileReader {
  
  def read(path: String) = {
    try {
    	for(line <- Source.fromFile(path).getLines()) {
    		println("Parse line: " + line)
    		    		
    	}
    } catch {
    	case ex: Exception => print(ex);
    }
  }

}