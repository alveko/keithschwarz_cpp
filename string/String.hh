/********************************************************** 
 * File: String.hh 
 * Author: Keith Schwarz (htiek@cs.stanford.edu) 
 * 
 * An implementation of a String class that uses several 
 * behind-the-scenes optimizations to improve performance. 
 * The main optimization is the small-string optimization. 
 * The key idea behind this optimization is that most strings 
 * are not particularly long; most are only a handful of 
 * characters in length.  Normally, strings are implemented 
 * by allocating an external buffer of characters.  In the 
 * small string optimization, the implementation instead works 
 * by using the object itself as the character buffer for 
 * small strings, switching over to an external buffer only 
 * when the size exceeds the object's preallocated storage 
 * space.  This is implemented in this class using a union of 
 * the two implementations. 
 */ 

#ifndef String_Included 
#define String_Included 

#include <iostream> // For ostream, istream 
#include <iterator> // For reverse_iterator 

class String { 
public: 
  /* Constructor: String(); 
   * Usage: String str; 
   * ------------------------------------------------------- 
   * Constructs a new, empty String. 
   */ 
  String(); 
   
  /* Constructor: String(char ch); 
   * Usage: String str = 'x'; 
   * ------------------------------------------------------ 
   * Constructs a String that is a single character in 
   * length. 
   */ 
  String(char ch); 
   
  /* Constructor: String(const char* str); 
   * Usage: String str = "This is a string!" 
   * ------------------------------------------------------- 
   * Constructs a new String whose contents are a deep-copy 
   * of the specified C-style string. 
   */ 
  String(const char* str); 

  /* Constructor: String(IterType start, IterType stop); 
   * Usage: String str(v.begin(), v.end()); 
   * ------------------------------------------------------- 
   * Constructs a new string whose elements are equal to the 
   * elements contained in the specified iterator range. 
   */ 
  template <typename IterType> 
  String(IterType start, IterType stop); 
   
  /* Destructor: ~String(); 
   * Usage: (implicit) 
   * ---------------------------------------------------- 
   * Deallocates the String and any resources it may have 
   * allocated. 
   */ 
  ~String(); 
   
  /* Copy constructor: String(const String& other); 
   * Usage: String str = otherString; 
   * ---------------------------------------------------- 
   * Creates a new String object that's a full, deep-copy 
   * of some other String object. 
   */ 
  String(const String& other); 
   
  /* Assignment operator: String& operator= (const String& other); 
   * Usage: str1 = str2; 
   * ------------------------------------------------------- 
   * Sets this String object equal to some other String object. 
   */ 
  String& operator= (const String& other); 
   
  /* Element access: char& operator[] (size_t index); 
   *                 char  operator[] (size_t index) const; 
   * Usage: cout << myStr[0] << endl; 
   * ------------------------------------------------------- 
   * Returns the character at the specified position in the 
   * String.  If the index is out of bounds, the behavior is 
   * undefined. 
   */ 
  char& operator[] (size_t index); 
  char  operator[] (size_t index) const; 
   
  /* Type: iterator 
   * Type: const_iterator 
   * ---------------------------------------------------- 
   * Types capable of traversing the elements of a String. 
   * iterators can read and write String contents, while 
   * const_iterators can only read contents.  Iterators can 
   * be invalidated by operations on the String.  In 
   * particular, all outstanding (const_)iterators are 
   * invalidated by any of the following: 
   * 
   * 1. A call to insert 
   * 2. A call to erase 
   * 3. A call to c_str 
   * 4. A call to swap 
   * 5. A call to operator= 
   * 6. A call to operator+= 
   * 7. A call to operator<< 
   */ 
  typedef char* iterator; 
  typedef const char* const_iterator; 

  /* Type: reverse_iterator 
   * Type: const_reverse_iterator 
   * ----------------------------------------------------- 
   * Types capable of traversing the elements of a String 
   * in reverse order.  The restrictions present on 
   * the standard iterator types also apply to the 
   * reverse_iterator type. 
   */ 
  typedef std::reverse_iterator<iterator> reverse_iterator; 
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator; 
   
  /* iterator begin(); 
   * const_iterator begin() const; 
   * Usage: for (String::iterator itr = s.begin(); itr != s.end(); ++itr) 
   * ---------------------------------------------------- 
   * Returns an iterator to the first element of the String. 
   */ 
  iterator begin(); 
  const_iterator begin() const; 
   
  /* iterator end(); 
   * const_iterator end() const; 
   * Usage: for (String::iterator itr = s.begin(); itr != s.end(); ++itr) 
   * ---------------------------------------------------- 
   * Returns an iterator to the element one step past the 
   * end of the String. 
   */ 
  iterator end(); 
  const_iterator end() const; 

  /* reverse_iterator rbegin(); 
   * const_reverse_iterator rbegin() const; 
   * Usage: for (String::reverse_iterator itr = s.rbegin(); itr != s.rend(); ++itr) 
   * ---------------------------------------------------- 
   * Returns an reverse_iterator to the last element of the String. 
   */ 
  reverse_iterator rbegin(); 
  const_reverse_iterator rbegin() const; 
   
  /* reverse_iterator rend(); 
   * const_reverse_iterator rend() const; 
   * Usage: for (String::reverse_iterator itr = s.rbegin(); itr != s.rend(); ++itr) 
   * ---------------------------------------------------- 
   * Returns an reverse_iterator to the element one step before the 
   * start of the String. 
   */ 
  reverse_iterator rend(); 
  const_reverse_iterator rend() const; 
   
  /* size_t size() const; 
   * bool empty() const; 
   * Usage: for (size_t i = 0; i < s.size(); ++i) 
   * ---------------------------------------------------- 
   * Returns the number of characters in the string and 
   * whether the string is empty, respectively. 
   */ 
  size_t size() const; 
  bool   empty() const; 
   
  /* iterator insert(iterator where, IterType start, IterType stop); 
   * Usage: s.insert(s.begin(), v.begin(), v.end()); 
   * ------------------------------------------------------- 
   * Inserts the range of characters [start, stop) into the 
   * String, beginning at position where.  Returns an iterator 
   * to the first element after all the inserted elements. 
   */ 
  template <typename IterType> 
  iterator insert(iterator where, IterType start, IterType stop); 

  /* iterator insert(iterator where, char what); 
   * Usage: s.insert(s.begin(), 'a'); 
   * ------------------------------------------------------- 
   * Inserts a single character into the String at the 
   * indicated position.  Returns an iterator to the newly- 
   * inserted element. 
   */ 
  iterator insert(iterator where, char what); 
   
  /* iterator erase(iterator where); 
   * Usage: s.erase(s.begin() + 4); 
   * ------------------------------------------------------- 
   * Removes a single character from the String at the 
   * indicated position.  Returns an iterator to the element 
   * one past the element that was erased (if any) or end() 
   * if the last element was removed. 
   */ 
  iterator erase(iterator where); 
   
  /* iterator erase(iterator start, iterator stop); 
   * Usage: s.erase(s.begin(), s.end());  
   * ------------------------------------------------------- 
   * Removes all of the elements in the range [start, stop) 
   * from the String, returning an iterator to the first 
   * element after the ones that were deleted. 
   */ 
  iterator erase(iterator start, iterator stop); 
   
  /* void swap(String& other); 
   * Usage: str1.swap(str2); 
   * ----------------------------------------------------- 
   * Exchanges the contents of this String object and some 
   * other String object.  This is guaranteed not to throw 
   * any exceptions. 
   */ 
  void swap(String& other); 
   
  /* const char* c_str() const; 
   * Usage: ifstream input(myStr.c_str()); 
   * ------------------------------------------------------- 
   * Returns a C-style string representation of this String 
   * object.  This C string is valid until any operation is 
   * performed on the String. 
   */ 
  const char* c_str() const; 
   
  /* size_t capacity() const; 
   * void reserve(size_t space); 
   * Usage: s.reserve(100); 
   *        if (s.capacity() >= 100) { ... } 
   * ------------------------------------------------------- 
   * capacity() returns the amount of space allocated in the 
   * String's internal storage.  Operations that do not 
   * increase the String's size beyond its capacity will 
   * proceed more quickly than operations that do.  The 
   * reserve(size_t) function ensures that the capacity is 
   * at least as large as its argument.  If you know in 
   * advance that the String will have a certain size, 
   * preemptively reserving space for the String can result 
   * in performance increases. 
   */ 
  size_t capacity() const; 
  void reserve(size_t space); 
   
  /* String& operator+= (const String& other); 
   * String& operator+= (char ch); 
   * Usage: one += two; 
   * ------------------------------------------------------- 
   * Concatenates the specified String onto the end 
   * of this String. 
   */ 
  String& operator+= (const String& other); 
   
private: 
  /* A constant dictating how many bytes are used for the small- 
   * string optimization.  Higher numbers mean less frequent 
   * allocations, but higher memory usage per string. 
   */ 
  static const size_t kMaxSmallStringSize = 31; 
   
  /* A struct encoding information necessary to implement a 
   * String using memory external to the string itself. 
   */ 
  struct LargeString { 
    char* elems; // Pointer to the elements buffer. 
    size_t logicalLength;   // How many characters are used. 
    size_t allocatedLength; // How much space is allocated. 
  }; 
   
  /* A struct encoding information necessary to implement a 
   * String using its own space for storage. 
   */ 
  struct SmallString { 
    /* We use just one byte to keep track of the storage space. */ 
    unsigned char size; 
     
    /* The rest of the bytes go to the preallocated buffer. */ 
    char elems[kMaxSmallStringSize]; 
  }; 
   
  /* A union of the two implementations.  For small strings, 
   * smallString is active; for large strings, largeString is 
   * active. 
   */ 
  union StringImpl { 
    LargeString largeString; 
    SmallString smallString; 
  }; 
   
  /* The implementation of this object, along with a flag controlling 
   * which implementation is being used. 
   */ 
  StringImpl impl; 
  bool isSmallString; 
}; 

/* Stream insertion and extraction. */ 
std::ostream& operator<< (std::ostream& out, const String& str); 
std::istream& operator>> (std::istream& in, String& str); 

/* Concatenation operator. */ 
const String operator + (const String& lhs, const String& rhs); 

/* Relational operators. */ 
bool operator <  (const String& lhs, const String& rhs); 
bool operator <= (const String& lhs, const String& rhs); 
bool operator == (const String& lhs, const String& rhs); 
bool operator != (const String& lhs, const String& rhs); 
bool operator >= (const String& lhs, const String& rhs); 
bool operator >  (const String& lhs, const String& rhs); 

/* Utility swap function in the std:: namespace. */ 
namespace std { 
  void swap(String& one, String& two); 
} 

/* * * * Implementation Below This Point * * * */ 

/* In an aggressively optimized implementation, the function to insert 
 * a range of iterators into the container would use some sort of template 
 * introspection to check whether the range of iterators is a single-pass 
 * or multipass range, and then perform some intelligent operations if 
 * the range is multipass.  For simplicitly, though, we will implement 
 * multi-element insert as multiple calls to single-element insert. 
 */ 
template <typename IterType> 
String::iterator String::insert(iterator where, IterType start, IterType stop) { 
  /* Keep advancing start toward stop until all elements have been inserted. 
   * At each step, insert the element, updating where so that it points to the 
   * newly-inserted element, and then advance it one step forward.  We need 
   * to use the return value of insert here in case the backing implementation 
   * reallocates space. 
   */ 
  for (; start != stop; ++start, ++where) 
    where = insert(where, *start); 
  return where; 
} 

/* The range constructor is implemented in terms of the multi-element insert function. */ 
template <typename IterType> 
String::String(IterType start, IterType stop) { 
  /* Initially, we are a small, empty string. */ 
  isSmallString = true; 
  impl.smallString.size = 0; 

  /* Add everything. */ 
  insert(begin(), start, stop); 
} 

#endif
