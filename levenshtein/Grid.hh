/*****************************************************************************
 * File: Grid.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A class representing a two-dimensional array of elements.  The elements can
 * be queried using the standard [] selection operators.
 *
 * Internally, the grid is represented as a one-dimensional std::vector of
 * elements stored in row-major order.  That is, given the grid
 *
 *                                0  1  2
 *                                3  4  5
 *                                6  7  8
 *                                9 10 11
 *
 * The elements would be stored in the vector as
 *
 *                    0  1  2  3  4  5  6  7  8  9 10 11
 *
 * The mapping from (row, col) to its corresponding position in the vector is
 * given by (nCols * row) + col, and the inverse mapping from a position i in
 * the vector to a (row, col) pair is (i / numRows, i % numRows).  This makes
 * it very efficient to determine which elements are where.
 */

#ifndef Grid_Included
#define Grid_Included

#include <vector>
#include <algorithm> // For lexicographical_compare

template <typename ElemType> class Grid {
public:
  /* Constructors either create an empty grid or a grid of the
   * specified size.
   */
  Grid();
  Grid(size_t numRows, size_t numCols);

  /* Resizing functions. */
  void clear();
  void resize(size_t width, size_t height);

  /* Size queries. */
  size_t numRows() const;
  size_t numCols() const;

  /* Queries for number of total elements and emptiness. */
  size_t size() const;
  bool   empty() const;

  /* Element access.  Because elements are stored in a std::vector, which (for
   * bools) does not always store the underlying type directly, the return type
   * of these accessor functions is the type stored in the vector rather than
   * the actual parameter type.
   */
  typename std::vector<ElemType>::reference getAt(size_t row, size_t col);
  typename std::vector<ElemType>::const_reference getAt(size_t row, size_t col) const;

  /* Iterator definitions are just vector iterators. */
  typedef typename std::vector<ElemType>::iterator iterator;
  typedef typename std::vector<ElemType>::const_iterator const_iterator;

  /* Container iteration. */
  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /* Row iteration. */
  iterator row_begin(size_t row);
  iterator row_end(size_t row);
  const_iterator row_begin(size_t row) const;
  const_iterator row_end(size_t row) const;

  /* Proxy objects for operator[] */
  class MutableReference;
  class ImmutableReference;

  /* Element selection functions. */
  MutableReference operator[](size_t row);
  ImmutableReference operator[](size_t row) const;

  /* Relational operators. */
  bool operator <  (const Grid& other) const;
  bool operator <= (const Grid& other) const;
  bool operator == (const Grid& other) const;
  bool operator != (const Grid& other) const;
  bool operator >= (const Grid& other) const;
  bool operator >  (const Grid& other) const;

private:
  std::vector<ElemType> elems;
  size_t rows;
  size_t cols;
};

/* * * * * Implementation Below This Point * * * * */

/* MutableReference and ImmutableReference implementations call back into the
 * grid that created them to index correctly.
 *
 * Both of these classes overload operator[] to actually select an element from
 * the grid.  This means that writing
 *
 *    myGrid[row][col]
 *
 * will be interepreted as "invoke operator[](col) on the (im)mutable reference
 * handed back by myGrid[row]."
 *
 * There are two versions of this class so that const-ness is preserved when
 * accessing grid elements.
 */
template <typename ElemType> class Grid<ElemType>::MutableReference {
public:
  typename std::vector<ElemType>::reference operator[](size_t col);

private:
  MutableReference(Grid* source, size_t row);
  Grid * ref;
  size_t row;

  /* Make Grid a friend so that it can invoke the constructor. */
  friend class Grid;
};
template <typename ElemType> class Grid<ElemType>::ImmutableReference {
public:
  typename std::vector<ElemType>::const_reference operator[](size_t col) const;
private:
  ImmutableReference(const Grid* source, size_t row);
  const Grid * ref;
  size_t row;

  friend class Grid;
};

/* Constructs a new, empty grid. */
template <typename ElemType> Grid<ElemType>::Grid() : rows(0), cols(0) {
  // Handled in initializer list
}

/* Constructs a grid of the specified size. */
template <typename ElemType>
Grid<ElemType>::Grid(size_t rows, size_t cols) :
  elems(rows * cols), rows(rows), cols(cols) {
  // Handled in initializer list
}

/* Empties the grid. */
template <typename ElemType> void Grid<ElemType>::clear() {
  elems.clear();
}

/* Discards all existing content and redimensions the grid. */
template <typename ElemType> void Grid<ElemType>::resize(size_t rows, size_t cols) {
  /* Resize the vector and fill its entries with default-constructed elements. */
  elems.assign(rows * cols, ElemType());
  this->rows = rows;
  this->cols = cols;
}

/* Size query functions. */
template <typename ElemType> size_t Grid<ElemType>::numRows() const {
  return rows;
}
template <typename ElemType> size_t Grid<ElemType>::numCols() const {
  return cols;
}

/* The size of the grid is the product of the number of rows and columns. */
template <typename ElemType> size_t Grid<ElemType>::size() const {
  return rows * cols;
}

/* The Grid is empty if the size is zero. */
template <typename ElemType> bool Grid<ElemType>::empty() const {
  return size() == 0;
}

/* Accessor member functions use the row-major order index conversions to access
 * elements.
 */
template <typename ElemType> typename std::vector<ElemType>::reference Grid<ElemType>::getAt(size_t row, size_t col) {
  return elems[col + row * cols];
}

template <typename ElemType>
typename std::vector<ElemType>::const_reference Grid<ElemType>::getAt(size_t row, size_t col) const {
  return elems[col + row * cols];
}

/* Iterator support, including const overloads. */
template <typename ElemType>
typename Grid<ElemType>::iterator Grid<ElemType>::begin() {
  return elems.begin();
}

template <typename ElemType>
typename Grid<ElemType>::const_iterator Grid<ElemType>::begin() const {
  return elems.begin();
}

template <typename ElemType>
typename Grid<ElemType>::iterator Grid<ElemType>::end() {
  return elems.end();
}

template <typename ElemType>
typename Grid<ElemType>::const_iterator Grid<ElemType>::end() const {
  return elems.end();
}

/* Row iteration uses the row-major order formula to select the proper elements. */
template <typename ElemType>
typename Grid<ElemType>::iterator Grid<ElemType>::row_begin(size_t row) {
  return elems.begin() + row * cols;
}

template <typename ElemType>
typename Grid<ElemType>::const_iterator Grid<ElemType>::row_begin(size_t row) const {
  return elems.begin() + row * cols;
}

template <typename ElemType>
typename Grid<ElemType>::iterator Grid<ElemType>::row_end(size_t row) {
  return row_begin(row) + cols;
}

template <typename ElemType>
typename Grid<ElemType>::const_iterator Grid<ElemType>::row_end(size_t row) const {
  return row_begin(row) + cols;
}

/* Implementation of the MutableReference and ImmutableReference classes. */
template <typename ElemType>
Grid<ElemType>::MutableReference::MutableReference(Grid* source, size_t row) :
  ref(source), row(row) {
  // Handled in initializer list
}

template <typename ElemType>
Grid<ElemType>::ImmutableReference::ImmutableReference(const Grid* source, size_t row) :
  ref(source), row(row) {
  // Handled in initializer list
}

/* operator[] calls back into the original Grid. */
template <typename ElemType>
typename std::vector<ElemType>::reference Grid<ElemType>::MutableReference::operator [](size_t col) {
  return ref->getAt(row, col);
}
template <typename ElemType>
typename std::vector<ElemType>::const_reference Grid<ElemType>::ImmutableReference::operator [](size_t col) const {
  return ref->getAt(row, col);
}
/* operator[] implementations create and return (Im)MutableReferences. */
template <typename ElemType>
typename Grid<ElemType>::MutableReference Grid<ElemType>::operator[](size_t row) {
  return MutableReference(this, row);
}
template <typename ElemType>
typename Grid<ElemType>::ImmutableReference Grid<ElemType>::operator[](size_t row) const {
  return ImmutableReference(this, row);
}

/* operator< performs a lexicographical comparison of two grids. */
template <typename ElemType>
bool Grid<ElemType>::operator <(const Grid& other) const {
  /* Lexicographically compare by dimensions. */
  if(rows != other.rows)
    return rows < other.rows;
  if(cols != other.cols)
    return cols < other.cols;

  /* Perform a lexicographical comparison. */
  return lexicographical_compare(begin(), end(), other.begin(), other.end());
}

/* Other relational operators defined in terms of operator<. */
template <typename ElemType>
bool Grid<ElemType>::operator >=(const Grid& other) const {
  return !(*this < other);
}
template <typename ElemType>
bool Grid<ElemType>::operator ==(const Grid& other) const {
  return !(*this < other) && !(other < *this);
}
template <typename ElemType>
bool Grid<ElemType>::operator !=(const Grid& other) const {
  return (*this < other) || (other < *this);
}
template <typename ElemType>
bool Grid<ElemType>::operator >(const Grid& other) const {
  return other < *this;
}
template <typename ElemType>
bool Grid<ElemType>::operator <= (const Grid& other) const {
  return !(other < *this);
}

#endif
