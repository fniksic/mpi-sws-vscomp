package mpidns

import java.net.ServerSocket
import scala.io.BufferedSource
import java.io.PrintStream
import mpidns.data.ZoneFileReader
import java.util.Calendar
import java.text.SimpleDateFormat
import java.net.DatagramSocket
import java.net.DatagramPacket

object DNSServer {

  val BUFFER_SIZE = 65536; // 64kB UDP package
  val PORT = 1024;

  def main(args: Array[String]): Unit = {

    // startup DNS server
    println("MPI-SWS DNS Server...startup on " + Calendar.getInstance().getTime())
    if (args.length <= 0) {
      MSG("Error: No zone file available")
      sys.exit()
    }
    MSG("Read zone file: " + args(0))

    // read given zone file
    val zone_records = ZoneFileReader.read(args(0))
    MSG("Zone file successfully parsed")
    //TODO: apply semantic checks here!

    // start server
    val sock = new DatagramSocket(PORT)
    val buf = new Array[Byte](BUFFER_SIZE)
    val packet = new DatagramPacket(buf, BUFFER_SIZE)

    MSG("DNS Server now listing on port: " + PORT)
    while (true) {
      
      sock.receive(packet)
      MSG("Request received from (" + packet.getAddress().toString.substring(1) + ")")

      //TODO: handle DSN request message!

      sock.send(packet)
      MSG("Anwser sent back to (" + packet.getAddress().toString.substring(1) + ")")
    }
  }

  // debugging only  
  def MSG(msg: String) = {
    val cal = Calendar.getInstance()
    val time_format = new SimpleDateFormat("HH:mm:ss")
    println(time_format.format(cal.getTime()) + " > " + msg)
  }
}