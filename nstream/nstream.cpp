/*
 * File: nstream.cpp
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 * Author: Prof. Eric Roberts (eroberts@cs.stanford.edu)
 * --------------------------------------------------
 * Implementation of the network stream class nstream.
 * The networking code here is based on that from
 * Beej's Guide to Networking, an excellent resource
 * on the subject.
 *
 * Internally, the nstream class is backed by a streambuf
 * called SocketStreambuf, which encapsulates a socket.
 * Its underflow and overflow methods receive and transmit
 * data over the socket, and its sync method flushes
 * the existing buffer.
 */

#include "nstream.hh"
#include <sstream>
#include <stdexcept>
#include <string>
#include <cstring>
using namespace std;

/* Utility function to convert an integer into a string. */
static string ShortToString(short value) {
  stringstream converter;
  converter << value;
  return converter.str();
}

/* Linux and Windows have slightly different conventions when working with sockets.
 * This code tries to mask as much of the difference as is reasonably possible.
 */
#ifdef _MSC_VER

  #include <winsock2.h>
  #include <ws2tcpip.h>

  typedef SOCKET SocketType;
  typedef int SocketLengthType;
  const SocketType NotASocket = INVALID_SOCKET;
  
  static void CloseSocket(SocketType socket) {
    closesocket(socket);
  }

  /* The socket APIs for Windows and Unix/Mac are similar in some ways, but one
   * major difference is that the Windows API requires some calls to functions at
   * program startup and shutdown.  This handles those cases.
   */
  static class WinsockInitialize {
  public:
    /* Tries to initialize WinSock, fails if unable to do so. */
    WinsockInitializer() {
      WSADATA wsaData;
      if(WSAStartup(MAKEWORD(2, 0), &wsaData ) != 0)
        throw runtime_error("Couldn't initialize WINSOCK.");
    
      /* Make sure the version is correct. */
      if(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 0)
        throw runtime_error("Incorrect WINSOCK version.");
    }
  
    ~WinsockInitializer() {
      WSACleanup();
    }
  } wsi;

#else

  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netdb.h>
  #include <arpa/inet.h>

  typedef int SocketType;
  typedef socklen_t SocketLengthType;
  const SocketType NotASocket = SocketType(-1);

  static void CloseSocket(SocketType socket) {
    close(socket);
  }

#endif

/* Stream buffer class that transports characters over the network. */
class nstream::SocketStreambuf: public streambuf {
public:
  /* Constructor that opens a connection to a remote server. */
  SocketStreambuf(const string& filename, const short portNum);
  
  /* Constructor that waits for an incoming connection from a foreign client. */
  explicit SocketStreambuf(const short portNum);
  
  /* Constructor that puts us into an empty state. */
  SocketStreambuf();

  /* Dtor cleans everything up. */
  ~SocketStreambuf();

  /* Returns whether things are happy on this end. */
  bool isGood() const {
    return good;
  }

  /* Returns whether a connection is open. */
  bool isOpen() const {
    return sock != NotASocket;
  }

  /* Two versions of open. */
  bool open(const short portNum);
  bool open(const string& hostName, const short portNum);

  /* Closes the socket connection, but first flushses any existing data.
   * If not open, reports an error.
   */
  bool close();

protected:
  /* Useful typedefs. */
  typedef traits_type::int_type int_type;
  typedef traits_type::pos_type pos_type;
  typedef traits_type::off_type off_type;

  /* On underflow, tries to read in more from the socket and fill things in. */
  virtual int_type underflow();

  /* On overflow, send everything we've got. */
  virtual int_type overflow(int_type value = traits_type::eof());

  /* Synchronizes the streams by flushing. */
  virtual int sync();

  /* Ungets a character.*/
  virtual int_type pbackfail(int_type ch);

private:

  /* Shuts down the connection and resets all fields to their initial state. */
  void shutdownConnection();

  /* The actual socket. */
  SocketType sock;

  /* Nice helper flag that tracks whether or not we're in a good state.
   * If not in a good state, we automatically fail every time we try to
   * read anything.
   */
  bool good;

  /* The actual buffer. */
  static const size_t kBufferSize = 4096;
  char readBuffer[kBufferSize];
  char writeBuffer[kBufferSize];
};

/* Ctor 1 is used to connect to a remote host. */
nstream::SocketStreambuf::SocketStreambuf(const string& hostname, const short portNum) :
  sock(NotASocket), good(false) {
  /* Try to set up the socket and fail if unable to do so. */
  open(hostname, portNum);
}

/* Ctor 2 is used to wait for an incoming connection. */
nstream::SocketStreambuf::SocketStreambuf(const short portNum) : sock(NotASocket), good(false) {
  /* Try to set up the connection and fail if unable to do so. */
  open(portNum);
}

/* Ctor 3 just puts us into an empty state. */
nstream::SocketStreambuf::SocketStreambuf() : sock(NotASocket), good(false) {
}

/* Destructor cleans up the socket. */
nstream::SocketStreambuf::~SocketStreambuf() {
  close();
}

void nstream::SocketStreambuf::shutdownConnection() {
  /* Time to close things, if we indeed have things to close. */
  if(sock != NotASocket)
    CloseSocket(sock);
  sock = NotASocket;
  good = false;
}

/* To close a connection, call sync and then reset things. */
bool nstream::SocketStreambuf::close() {
  if(!isOpen()) return false;

  sync();
  shutdownConnection();
  return true;
}

/* Try connecting on the specified address/port combination. */
bool nstream::SocketStreambuf::open(const string& address, const short portNum) {
  try {
    /* Now, try to look up the target.  Begin by setting up hints accordingly. */
    addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    /* Hold the result of the getaddrinfo that listens on the indicated port. */
    addrinfo* addresses = NULL;
    const int result = getaddrinfo(address.c_str(), 
                                   ShortToString(portNum).c_str(), 
                                   &hints, &addresses);
    if(result != 0)
      throw runtime_error("Couldn't get address info for this server.");
    
    try  {
      /* Make sure they exist! */
      if(addresses == NULL)
        throw runtime_error("Got server info, but there were no nodes.");
      
      /* Begin by initializing the socket. */
      sock = socket(addresses->ai_family, addresses->ai_socktype, addresses->ai_protocol);
      if(sock == -1)
        throw runtime_error("Couldn't create local socket.");
      
      /* Now for the fun part - actually connect! */
      if(connect(sock, addresses->ai_addr, (int) addresses->ai_addrlen) == -1)
        throw runtime_error("Unable to connect to server.");
      
      /* Once done, make sure to clean things up! */
      freeaddrinfo(addresses);
           
      /* Yay!  We're good. */
      good = true;
    } catch(const runtime_error &){
      /* On error, reclaim resources. */
      if(addresses)
        freeaddrinfo(addresses);
      if(sock != NotASocket)
        CloseSocket(sock);
      throw;
    }
  } catch(const runtime_error &) {
    sock = NotASocket;
    return false;
  }
  return true;
}

/* Try waiting for an incoming connection. */
bool nstream::SocketStreambuf::open(const short portNum) {
  /* Begin by setting up information about our machine. */
  addrinfo hints, *addresses = NULL;
  memset(&hints, 0, sizeof(addrinfo));
  
  /* Tell us to allow connections to any IP address on any protocol. */
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  
  /* Call getaddrinfo to learn where to wait. */
  if(getaddrinfo(NULL, ShortToString(portNum).c_str(), &hints, &addresses) != 0)
    return false;
  
  try {
    /* Set up a socket that just sits around waiting for incoming connections.  If
     * at any point we fail, abort.
     */
    SocketType listenerSocket = NotASocket;
    try {
      /* Get a socket to listen with.  Fail if we can't. */
      listenerSocket = socket(addresses->ai_family,
                              addresses->ai_socktype,
                              addresses->ai_protocol);
      if(listenerSocket == NotASocket)
        throw runtime_error("Couldn't create a listener socket.");
      
      /* Bind the socket on the proper port so we can listen.  Fail if we can't. */
      if(bind(listenerSocket, addresses->ai_addr, (int) addresses->ai_addrlen) != 0)
        throw runtime_error("Couldn't bind the socket.");
      
      /* Now, tell the socket to listen in for at most one connection. */
      if(listen(listenerSocket, 1) == -1)
        throw runtime_error("Couldn't listen for incoming connections.");
      
      /* Finally, set our socket to be the one we get when we find someone who
       * wants to talk to us.
       */
      SocketLengthType addrSize = sizeof (sockaddr_storage);
      sockaddr_storage theirAddr;
      sock = accept(listenerSocket, (struct sockaddr *)&theirAddr, &addrSize);
      if(sock == NotASocket)
        throw runtime_error("Failed to accept a connection.");
      
      /* Yay!  We're listening in!  Now close the listener socket and set us status
       * to "success."
       */
      CloseSocket(listenerSocket);
      good = true;
    } catch(const runtime_error &){
      /* Whoops!  Something went wrong.  Clean up the socket and keep unwinding
       * to execute other cleanup.
       */
      if(listenerSocket != NotASocket)
        CloseSocket(listenerSocket);
      throw;
    }
    
    /* Finally, clean up the address info data. */
    freeaddrinfo(addresses);
  } catch(const runtime_error &) {
    /* Remember to clean up the address info.  This is why C++ > C.
     * Use your destructors, kids.
     */
    if(addresses)
      freeaddrinfo(addresses);
    sock = NotASocket;
    return false;
  }
  return true;
}

/* On an overflow, we need to send all of the data we have. */
nstream::SocketStreambuf::int_type
nstream::SocketStreambuf::overflow(SocketStreambuf::int_type value)
{
  /* First, if things are bad, fail immediately. */
  if(!good)
    return traits_type::eof();

  /* Continuously send data until we've exhausted the buffer.  Since
   * the socket might not send everything all at once, loop until we
   * have successfully transmitted everything.
   */
  const char* readPtr = pbase();
  while(readPtr != pptr()) {
    /* Send what we can. */
    const int sendResult = send(sock, readPtr, (size_t)(pptr() - readPtr), 0);
    if(sendResult == -1){
      /* Ouch!  We broke.  How sad.  Now we'll set our put area to
       * empty and go into a fail state.
       */
      setp(NULL, NULL);
      good = false;
      return traits_type::eof();
    }
    
    /* Advance forward for the next write. */
    readPtr += sendResult;
  }
  
  /* Now, at the end, we've successfully transmitted.  We now need to
   * reset the put area.  Also, if the char we got wasn't EOF, we should
   * put that as the first char.
   */
  setp(writeBuffer, writeBuffer + kBufferSize);

  if(!traits_type::eq_int_type(traits_type::eof(), value))
    sputc(value);

  /* Success! */
  return traits_type::not_eof(value);
}

/* To synchronize the buffer and the socket, we just overflow with EOF
 * to force a write.
 */
int nstream::SocketStreambuf::sync() {
  return traits_type::eq_int_type(overflow(traits_type::eof()), traits_type::eof()) ? -1 : 0;
}

/* To receive data, we'll call recv and wait for things to show up. */
nstream::SocketStreambuf::int_type nstream::SocketStreambuf::underflow() {
  /* If we are in a fail state, automatically abort. */
  if(!good)
    return traits_type::eof();

  if(gptr() == egptr()) {
    int numRead = recv(sock, readBuffer, kBufferSize, 0);
    
    /* Check that we read good things.  0 or -1 indicates a closed connection. */
    if(numRead == -1 || numRead == 0) {
      good = false; // Fail all future reads.
      return traits_type::eof();
    }
    
    /* Okay, we read things.  Now set the pointers up. */
    setg(readBuffer, readBuffer, readBuffer + numRead);
  }
  return *gptr();
}

/* Put back the character, unless we're out of room. */
nstream::SocketStreambuf::int_type nstream::SocketStreambuf::pbackfail(int_type ch) {
  /* If we don't have room, fail immediately. */
  if (eback() == egptr())
    return traits_type::eof();

  /* Back up a step. */
  gbump(-1);

  /* If the character isn't EOF, write it back. */
  if (traits_type::eq_int_type(ch, traits_type::eof()))
    *gptr() = ch;

  /* Return something other than EOF to signal success. */
  return traits_type::not_eof(ch);
}

/* nstream constructors initialize the data members, tell the superclass
 * ctor that all is well, and tie itself to itself so that reads force a write.
 */

/* Client constructor */
nstream::nstream(string hostName, short portNum) :
  iostream(NULL), connection(new SocketStreambuf()) {
  init(connection);

  /* We tie the stream to itself so that if we try to read,
   * we first flush all of the buffered data.  Otherwise,
   * an explicit flush would be required before any read.
   */
  tie(this);
  
  open(hostName, portNum);
}

/* Server constructor. */
nstream::nstream(short portNum) :
  iostream(NULL), connection(new SocketStreambuf(portNum)) {
  init(connection);
  tie(this);

  open(portNum);
}

/* Default constructor. */
nstream::nstream() : iostream(NULL), connection(new SocketStreambuf) {
  init(connection);
  tie(this);
}

nstream::~nstream() {
  rdbuf(0); // Avoid weirdness if superclass tries to use the buffer.
  delete connection;
}

/* Opens a connection to a remote server, failing if unable to do so. */
void nstream::open(string filename, short portNum) {
  if(!connection->open(filename, portNum))
    setstate(ios_base::failbit);
}

/* Opens a connection and waits for a connection, failing if unable to do so. */
void nstream::open(short portNum) {
  if(!connection->open(portNum))
    setstate(ios_base::failbit);
}

/* Closes the connection. */
void nstream::close() {
  if(!connection->close())
    setstate(ios_base::failbit);
}

/* Returns whether the stream is open. */
bool nstream::is_open() const {
  return connection->isOpen();
}
