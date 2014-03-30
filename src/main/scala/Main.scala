package harp

object Main {
  def main(a: Array[String]) {
    val conf = Parse("""
    |# global config goes here
    |global
    |  maxconn 4096
    |  pidfile ~/tmp/haproxy-queue.pid
    |  stats socket /tmp/haproxy.stat mode 600
    |
    |defaults
    | log global
    |  log 127.0.0.1 local0
    |  log 127.0.0.1 local1 notice  
    |  mode http
    |  timeout connect 300000
    |  timeout client 300000
    |  timeout server 300000
    |  option httpchk HEAD / HTTP/1.0
    |
    |frontend http-farm
    |  # binds to port 9000
    |  # then listens of course
    |  bind *:9000
    |  default_backend app1latest
    |  acl url_tag02 path_beg /tag02/
    |  use_backend tagged-02 if url_tag02
    |
    |backend app1latest
    |  balance roundrobin
    |  server localhost_9001 localhost:9001
    |
    |listen haproxyapp_admin:9100 127.0.0.1:9100
    |  mode http
    |  stats uri /""".stripMargin)

    for {
      cfg  <- conf
      fe   <- cfg.frontend("http-farm")
      opt <- fe.get("acl")
    } println(opt)
  }
}
