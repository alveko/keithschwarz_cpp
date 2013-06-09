/*****************************************************************
 * File: String.cpp
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of the String type defined in String.hh.
 */

#include "String.hh"

/* Implementation-specific includes. */
#include <algorithm>
#include <cctype>
#include <cstring>
#include <iterator>

/* Default constructor sets up a small string with no elements. */
String::String() {
  isSmallString = true;
  impl.smallString.size = 0;
}

/* Character constructor sets up a small string with one element. */
String::String(char ch) {
  isSmallString = true;
  impl.smallString.size = 1;
  impl.smallString.elems[0] = ch;
}

/* Conversion constructor tries to use a small string if possible,
 * but falls back on a large string if this will not work.
 */
String::String(const char* str) {
  /* Compute the length of the string once, since strlen can be
   * expensive.
   */
  const size_t length = std::strlen(str);
  
  /* We will be a small string if the number of characters does not
   * exceed the buffer size.
   */
  isSmallString = (length <= kMaxSmallStringSize);
  
  /* If we aren't a small string, allocate external storage and transfer
   * the C string.
   */
  if (!isSmallString) {
    impl.largeString.logicalLength   = length;
    impl.largeString.allocatedLength = length;
    impl.largeString.elems = new char[impl.largeString.allocatedLength];
  }
  else
    impl.smallString.size = (unsigned char)length;              
  
  /* Copy the string onto our object using the standard copy algorithm.
   * The begin() function is smart enough to determine whether we are a small or
   * large string, and will hand back the correct pointer in either case.
   *
   * We do not use the strcpy function here because the type of begin() is
   * String::iterator.  Although currently String::iterator is a shallow
   * typedef for char*, in future implementations this may not be the case.
   * For example, we may want to change the implementation to have a checked
   * iterator that ensures we don't walk off the end of the buffer.
   */
  std::copy(str, str + length, begin());
}

/* String copy constructor is a bit tricky.  We want to ensure that we copy over
 * the data without having any shared references, but at the same time don't want
 * to lose the small string optimization.
 */
String::String(const String& other) {
  /* Copy over whether we're a small string. */
  isSmallString = other.isSmallString;
  
  /* If we're a large string, allocate an external buffer for the string. */
  if (!isSmallString) {
    impl.largeString.logicalLength = other.impl.largeString.logicalLength;
    impl.largeString.allocatedLength = other.impl.largeString.allocatedLength;
    impl.largeString.elems = new char[impl.largeString.allocatedLength];
  }
  /* Otherwise, copy over the string size. */
  else
    impl.smallString.size = other.impl.smallString.size;
  
  /* Finally, copy over the string itself.  Note that we go through the public
   * interface to do so.
   */
  std::copy(other.begin(), other.end(), begin());
}

/* This assignment operator is implemented using the copy-and-swap pattern.  If
 * you are not familiar with this pattern, consider reading "Effective C++, 3rd Edition"
 * by Scott Meyers, which has a great explanation of the technique.
 */
String& String::operator= (const String& other) {
  String copy(other);
  swap(copy);
  return *this;
}

/* The String destructor only needs to clean up memory if we are using a large
 * string.
 */
String::~String() {
  if (!isSmallString)
    delete [] impl.largeString.elems;
}

/* The begin() function returns a pointer to the start of the buffer.  This may be
 * the internal buffer for the small string optimization, or it may be the external
 * buffer.  By hiding this complexity here, the implementation can have a more
 * unified treatment of the two implementations.
 */
String::iterator String::begin() {
  return isSmallString? impl.smallString.elems : impl.largeString.elems;
}
String::const_iterator String::begin() const {
  return isSmallString? impl.smallString.elems : impl.largeString.elems;
}

/* The end iterator is implemented in terms of begin() and size(), which helps
 * to mask the implementation complexity.
 */
String::iterator String::end() {
  return begin() + size();
}
String::const_iterator String::end() const {
  return begin() + size();
}

/* reverse_iterator support is implemented in terms of regular iterators. */
String::reverse_iterator String::rbegin() {
  return reverse_iterator(end());
}
String::const_reverse_iterator String::rbegin() const {
  return const_reverse_iterator(end());
}
String::reverse_iterator String::rend() {
  return reverse_iterator(begin());
}
String::const_reverse_iterator String::rend() const {
  return const_reverse_iterator(begin());
}

/* Individual element selection is similarly implemented in terms of begin().  As
 * you can see, pushing the complexity to begin() greatly simplifies the rest of
 * the implementation.
 */
char& String::operator[] (size_t index) {
  return *(begin() + index);
}
char String::operator[] (size_t index) const {
  return *(begin() + index);
}

/* In order to convert the string into a C-style string, we need to allocate a buffer
 * and append a null terminator.  We'll use our own buffer for this, which may
 * trigger a reallocation.  However, getting a C-style string is semantically const,
 * and so we will need to use a const_cast to trick the compiler into thinking that
 * we're doing something safe.
 */
const char* String::c_str() const {
  /* Ensure space exists for a new character.  Since reserve isn't const,
   * we'll need to fake it here.
   */
  String* me = const_cast<String*>(this);
  me->reserve(size() + 1);
  
  /* Normally, dereferencing end() is a bad idea.  However, the last call to
   * reserve guarantees that the space exists, so this is perfectly safe.
   */
  *me->end() = 0;
  
  /* It's unclear which implementation we're using here, but fortunately
   * begin() knows what to do.
   */
  return begin();
}

/* Global swap function in namespace std just calls the class's version of that
 * function.
 */
void std::swap(String& one, String& two) {
  one.swap(two);
}

/* The capacity of the string is:
 * 1. kMaxSmallStringSize if the string is small.
 * 2. Found in the LargeString object otherwise.
 */
size_t String::capacity() const {
  return isSmallString? kMaxSmallStringSize : impl.largeString.allocatedLength;
}

/* The size may be in one of two places depending on the implementation,
 * so we much check how we are implemented first.
 */
size_t String::size() const {
  return isSmallString? impl.smallString.size : impl.largeString.logicalLength;
}

/* We're empty if we have size zero.  Note that implementing empty() in terms of
 * size is generally not a good idea, but since size() runs in O(1) this is both
 * safe and a good idea from an encapsulation perspective.
 */
bool String::empty() const {
  return size() == 0;
}

/* reserve is one of the trickiest functions in this implementation.
 * We need to be sure to avoid leaking memory if we reallocate an external
 * buffer, but also have to make sure that we don't free the internal
 * buffer.
 */
void String::reserve(size_t newCapacity) {
  /* If we already have the space requested, do nothing. */
  if (newCapacity <= capacity())
    return;
  
  /* Rather than just going with the specified capacity, we'll continuously
   * double the buffer size until we have space.  This ensures that, in an
   * amortized sense, appending single characters takes O(1) apiece.
   */
  size_t currSize = size();
  size_t newSize = capacity();
  while (newSize < newCapacity)
    newSize *= 2;
  
  /* We're going to assume that if we need to reallocate space, we can no longer
   * use the small string optimization.  The rationale for this is that we only
   * get here if the requested size is bigger than the current capacity, and the
   * capacity defaults to kMaxSmallStringSize for small strings.  Consequently,
   * we'll begin by allocating a buffer for the characters here.
   */
  char* newElems = new char[newSize];
  
  /* Copy over our data. */
  std::copy(begin(), end(), newElems);
  
  /* This is the tricky part.  If we are currently a large string, we need to
   * deallocate the buffer we currently reference.
   */
  if (!isSmallString)
    delete [] impl.largeString.elems;
  /* Otherwise, if we are transitioning from a small string to a large string,
   * we must copy over the size, since the location of the string's size
   * changes.
   */
  else
    impl.largeString.logicalLength = currSize;
  
  /* We are no longer a small string. */
  isSmallString = false;
  
  /* Store the new size and capacity. */
  impl.largeString.allocatedLength = newSize;
  impl.largeString.elems = newElems;
}

/* To insert a character, we shuffle down all the elements after the insert
 * point, then drop the character in the old spot.
 */
String::iterator String::insert(iterator where, char what) {
  /* We need to ensure that space exists for the element, so we will call reserve.
   * However, this has the potential to reallocate the buffer, in which case our 
   * iterator will be pointing into an invalid buffer.  Consequently, we'll cache
   * where in the container the iterator points, call reserve, and then rebuild
   * the iterator.
   */
  ptrdiff_t offset = where - begin();
  reserve(size() + 1);
  where = offset + begin();
  
  /* Shuffle down all of the elements after the insert point.  We use
   * std::copy_backwards instead of std::copy since the destination
   * point of the copy overlaps the range of elements to copy.
   */
  std::copy_backward(where, end(), end() + 1);
  
  /* Write the value. */
  *where = what;
  
  /* Update our size accordingly.  Since there are two positions
   * where the size might be stored, we need twice as much logic as
   * usual.
   */
  if (isSmallString)
    ++impl.smallString.size;
  else
    ++impl.largeString.logicalLength;
  
  /* Return an iterator to the element we just inserted.  Notice
   * that since we rebuilt the iterator after the reserve, this iterator
   * will be valid.
   */
  return where;
}

/* erase just shuffles elements down. */
String::iterator String::erase(iterator start, iterator stop) {
  /* See how many elements we're erasing. */
  const ptrdiff_t numToRemove = stop - start;

  /* Shuffle the elements numToRemove positions after start down on top
   * of start.
   */
  std::copy(start + numToRemove, end(), start);

  /* Decrease the size of the String. */
  if (isSmallString)
    impl.smallString.size -= numToRemove;
  else
    impl.largeString.logicalLength -= numToRemove;
  
  /* Hand back an iterator to the element one step past the last one that was deleted.
   * Since this element now occupies the same space as the element that was deleted,
   * we just hand back the iterator the client passed in.
   */
  return start;
}

/* Single-element erase function implemented in terms of the multi-element erase. */
String::iterator String::erase(iterator where) {
  return erase(where, where + 1);
}

/* Our implementation of swap simply calls the STL version of this method on each data
 * member.
 */
void String::swap(String& other) {
  std::swap(isSmallString, other.isSmallString);
  
  /* Notice that we don't need any special logic to swap the implementations
   * based on whether one is a small string, etc.  By swapping all of the bytes
   * in the union, we're guaranteed to copy everything over correctly.
   */
  std::swap(impl, other.impl);
}

/* String append operator is in many ways similar to insert, except that we don't need
 * to shuffle any elements down.
 */
String& String::operator+= (const String& rhs) {
  /* Ensure space exists. */
  reserve(size() + rhs.size());
  
  /* Copy the elements from the parameter onto the end of this string. */
  std::copy(rhs.begin(), rhs.end(), end());
  
  /* Increase the size. */
  if (isSmallString)
    impl.smallString.size += rhs.size();
  else
    impl.largeString.logicalLength += rhs.size();
  
  /* operator+= produces an lvalue, so we return a reference to ourselves. */
  return *this;
}

/* This implementation of operator+ is based entirely on the String copy constructor
 * and operator+= function.  This is a fairly standard trick that allows for a large
 * amount of code reuse.
 */
const String operator+ (const String& lhs, const String& rhs) {
  return String(lhs) += rhs;
}

/* To write the String out to a stream, we simply output it's C string representation.
 * To be more formal, we should parameterize this function over the type of the
 * character and character traits of the output stream, but for simplicity we don't
 * do this here.
 */
std::ostream& operator<< (std::ostream& out, const String& str) {
  return out << str.c_str();
}

/* The stream input operator is a bit tricky.  We continuously read characters until
 * we find a space character or the end of the stream.
 */
std::istream& operator>> (std::istream& in, String& str)  {
  /* Because stream operators should not modify their arguments unless the operation
   * succeeds, we buffer the result into a local String and then copy the result over
   * if the operation succeeds.
   */
  String result;

  /* Build a sentry object.  This will check that the stream is valid and will skip
   * any leading whitespace.
   */
  std::istream::sentry s(in);
  if (s) {
    /* Keep reading characters until we get a space or EOF. */
    char ch;
    while (in.get(ch)) {
      /* Check if we got any whitespace.  Normally, this would be done using locales, but
       * for simplicity we'll use the basic isspace from <cctype>.
       */
      if (std::isspace(ch)) {
        /* Put the character back so that we can read it in in a future read. */
        in.unget();
        break;
      }
      result += ch;
    }

    /* If we're here, we either hit a space or EOF.  In either case,
     * we need to set the outparam to be the String we just read.  We'll do
     * this by using the swap function.
     */
    std::swap(str, result);
  }

  return in;
}

/* The implementation of < is a lexicographical comparison, done using the STL's
 * lexicographical_compare algorithm.
 */
bool operator < (const String& one, const String& two) {
  return std::lexicographical_compare(one.begin(), one.end(), two.begin(), two.end());
}

/* Equality comparison is done by checking that the sizes agree and that the characters
 * are all equal.
 */
bool operator== (const String& one, const String& two) {
  return one.size() == two.size() && std::equal(one.begin(), one.end(), two.begin());
}

/* The rest of the relational operators are implemented in terms of < and ==. */
bool operator <= (const String& one, const String& two)  {
  return !(two < one);
}

bool operator != (const String& one, const String& two) {
  return !(one == two);
}

bool operator >= (const String& one, const String& two) {
  return !(one < two);
}

bool operator > (const String& one, const String& two) {
  return two < one;
}
