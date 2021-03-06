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
import java.io.PrintWriter
import java.io.FileWriter
import mpidns.data.AnswerTreeNode
import mpidns.data.RR
import mpidns.data.RR_A
import mpidns.data.RR_MX
import mpidns.data.RR_NS
import mpidns.data.RR_PTR
import mpidns.data.RR_TXT
import mpidns.data.RR_SOA
import mpidns.data.RR_CNAME
import mpidns.data.RR_CNAME
import mpidns.data.PlainTreeHelpers

object DNSServer {

  val BUFFER_SIZE = 65536; // 64kB UDP packet
  val PORT = 2048;

  private var i = 0
  private def RRtoDOT(io: PrintWriter)(rr: RR): Unit = {
    rr match {
      case RR_A(_, a) => io.println("  n" + i + " [shape=none,label=\"A: " + a + "\"];")
      case RR_MX(_, p, n) =>
        io.println("  n" + i + " [shape=none,label=\"MX: " + n + "@" + p + "\"];")
      case RR_NS(_, n, a) =>
        io.println("  n" + i + " [shape=none,label=\"NS: " + n + " " + a + "\"];")
      case RR_PTR(_, n) =>
        io.println("  n" + i + " [shape=none,label=\"PTR: " + n + "\"];")
      case RR_TXT(_, n) =>
        io.println("  n" + i + " [shape=none,label=\"TXT: " + n + "\"];")
      case RR_SOA(_, dn, hm, s, t1, t2, t3, t4) =>
        io.println("  n" + i + " [shape=none,label=\"SOA: " + dn +
          "; " + hm + ";" + s + ";" + t1 + ";" + t2 + ";" + t3 + ";" + t4 + "\"];")
      case RR_CNAME(_, n, extra) =>
        io.println("  n" + i + " [shape=none,label=\"CNAME: " + n + " " +
          rr.asInstanceOf[RR_CNAME].flatten_extra + "\"];")

    }
  }
  private def answerTreeToDOT(io: PrintWriter)(node: AnswerTreeNode): Unit = {
    if (node.authoritative) {
      io.println("  n" + i + " [shape=\"rectangle\"]")
    }
    val me: Int = i
    for (rr <- node.rrs) yield {
      i = i + 1
      io.println("  n" + me + " -> n" + i + ";")
      RRtoDOT(io)(rr)
    }
    for ((name, child) <- node.children) {
      println("Handling " + name)
      i = i + 1
      io.println("  n" + me + " -> n" + i + ";")
      io.println("  n" + i + " [label=\"" + name + "\"];")
      answerTreeToDOT(io)(child)
    }
  }

  private def dumpTreeToDOT(answer_tree: mpidns.data.AnswerTree): Unit = {
    val atwriter: PrintWriter = new PrintWriter(new FileWriter("answertree.dot"))
    atwriter.println("digraph answertree {")
    atwriter.println("  n0 [label=\".\"];")
    answerTreeToDOT(atwriter)(answer_tree.root)
    atwriter.println("}")
    atwriter.close()
  }

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

    val plain_tree = PlainTreeHelpers.buildPlainTree(zone_records)
    val answer_tree = BuildAnswerTree.build_answer_tree(plain_tree) match {
      case Left(at) => at
      case Right(err) =>
        {
          MSG("Error: bad zone file")
          err.foreach({ e => println(e.n + ": " + e.msg) })
        }
        sys.exit() // TODO dump error messages
    }
    dumpTreeToDOT(answer_tree)

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
      val response_buffer = Compression.messageToBytes(response_msg)
      val response_packet = new DatagramPacket(response_buffer, Math.min(BUFFER_SIZE, response_buffer.length))
      response_packet.setSocketAddress(query_packet.getSocketAddress())
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
