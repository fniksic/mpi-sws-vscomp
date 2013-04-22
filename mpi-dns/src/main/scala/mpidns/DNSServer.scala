package mpidns

import java.net.ServerSocket
import scala.io.BufferedSource
import java.io.PrintStream
import mpidns.data.ZoneFileReader
import java.util.Calendar
import java.text.SimpleDateFormat
import java.net.DatagramSocket
import java.net.DatagramPacket
import mpidns.data.BuildAnswerTree
import mpidns.data.EmptyPlainTree
import mpidns.data.PlainTree
import mpidns.data.PlainRR
import mpidns.data.Name
import mpidns.data.Compression

object DNSServer {

  val BUFFER_SIZE = 65536; // 64kB UDP packet
  val PORT = 1024;

  def ptbuild (pt: PlainTree, d: (String, PlainRR)): PlainTree =
    d match { case (n, rr) => return  pt.addRR(new Name(n), rr) }
    
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
    
    val plain_tree = zone_records.foldLeft(EmptyPlainTree: PlainTree)(ptbuild)
    val answer_tree = BuildAnswerTree.build_answer_tree(plain_tree) match {
      case Left(at) => at
      case Right(err) => {
        MSG("Error: bad zone file")
        err.foreach({e => println(e.n + ": " + e.msg)})
      }
      sys.exit() // TODO dump error messages
    }
    // initiate resolver with given answer tree
    val resolver = new Resolver(answer_tree)
    
    // start server
    val sock = new DatagramSocket(PORT)
    val query_buffer = new Array[Byte](BUFFER_SIZE)
    val query_packet = new DatagramPacket(query_buffer, BUFFER_SIZE)

    MSG("DNS Server now listing on port: " + PORT)
    while (true) {      
      // listen for incoming DNS queries
      sock.receive(query_packet)
      MSG("Request received from (" + query_packet.getAddress().toString.substring(1) + ")")
      
      //TODO: decode query_buffer into query message
      val query_msg = Compression.bytesToMessage(query_packet.getData())
      val response_msg = resolver.resolve(query_msg)
      
      //TODO: encode response message into response_buffer
      val response_buffer = Compression.messageToBytes(response_msg)
      val response_packet = new DatagramPacket(response_buffer, BUFFER_SIZE)
      response_packet.setSocketAddress(response_packet.getSocketAddress())
      sock.send(response_packet)
      MSG("Anwser sent back to (" + response_packet.getAddress().toString.substring(1) + ")")
    }
  }

  // debugging only  
  def MSG(msg: String) = {
    val cal = Calendar.getInstance()
    val time_format = new SimpleDateFormat("HH:mm:ss")
    println(time_format.format(cal.getTime()) + " > " + msg)
  }
}