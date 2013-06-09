/*********************************************************************
 * File: Matrix.h
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A pair of classes - Matrix and Vector - which represent mathematical
 * matrices and vectors.  Matrices and vectors can be multiplied, added,
 * subtracted, etc. with their standard interpretations.  Unlike many
 * matrix libraries, these matrices use left-multiplication like the
 * standard mathematical definition.
 *
 * These matrix classes are designed for fixed-sized matrices whose sizes
 * are known at compile-time.  Consequently, the type system will ensure
 * that matrix operations that are undefined cannot be performed by these
 * matrix/vector classes.
 */

#ifndef Matrix_Included
#define Matrix_Included

#include <stdlib.h>
#include <ostream>
#include <functional>
#include <algorithm>

/* Type: Matrix
 * ---------------------------------------------------------------------
 * A type representing an arbitrary matrix.  The matrix is parameterized
 * over its rows, colums, and element type (which defaults to double).
 */
template <size_t Rows, size_t Cols, typename ElemType = double>
class Matrix {
public:
  /* Constructor: Matrix()
   * Usage: Matrix<3, 3> m;
   * -----------------------------------------------------------------
   * Constructs a new matrix of the requisite dimensions.  Elements are
   * not initialized, so make sure that you set the Matrix to a reasonable
   * default.
   */
  Matrix();

  /* Constructor: Matrix(InputIterator begin, InputIterator end);
   * Usage: Matrix<3, 3> m(v.begin(), v.end())
   * -----------------------------------------------------------------
   * Constructs a matrix out of a range of values specified by an iterator
   * range.  Elements are assumed to be in row-major order.  The behavior
   * is undefined if the iterator range does not contain exactly (Width *
   * Height) elements.
   */
  template <typename InputIterator> 
  Matrix(InputIterator begin, InputIterator end);

  /* Function: ElemType& at(size_t row, size_t col)
   *           const ElemType& at(size_t row, size_t col) const;
   * Usage: m.at(2, 2) = 137.0;
   * -----------------------------------------------------------------
   * Retrieves a single element in the Matrix.  No bounds-checking is
   * performed.
   */
  ElemType& at(size_t row, size_t col);
  const ElemType& at(size_t row, size_t col) const;

  /* Function: size_t numRows() const;
   * Function: size_t numCols() const;
   * Usage: for (size_t i = 0; i < m.numRows(); ++i)
   * -----------------------------------------------------------------
   * Returns the number of rows or columns in the matrix.  Because these
   * values are specified as template arguments, they are mostly convenience
   * methods.
   */
  size_t numRows() const;
  size_t numCols() const;
  
  /* Function: size_t size() const;
   * Usage: std::fill_n(m.begin(), m.size(), 0.0);
   * -----------------------------------------------------------------
   * Returns the total number of elements in the Matrix.  This is equal
   * to numRows() * numCols().
   */
  size_t size() const;

  /* Pseudofunction: ElemType& operator[][](size_t row, size_t col);
   *                 const ElemType& operator[][](size_t row, size_t col);
   * Usage: m[0][0] = 137.0;
   * -----------------------------------------------------------------
   * Implementation of multidimensional element selection.  This allows
   * you to treat the Matrix as through it were a two-dimensional array.
   */
  class MutableReference;
  class ImmutableReference;
  MutableReference operator[] (size_t row);
  ImmutableReference operator[] (size_t row) const;

  /* Type: iterator
   * Type: const_iterator
   * -----------------------------------------------------------------
   * STL-compatible iterator and const_iterator types.  These iterators
   * traverse the Matrix in row-major order.
   */
  typedef ElemType* iterator;
  typedef const ElemType* const_iterator;

  /* Function: iterator begin();
   *           const_iterator begin() const;
   * Function: iterator end();
   *           const_iterator end() const;
   * Usage: for (Matrix<3, 3>::iterator itr = m.begin(); itr != m.end(); ++itr)
   * -----------------------------------------------------------------
   * Returns iterators delineating the entire contents of the Matrix.
   * These iterators traverse each row in succession from top to bottom.
   */
  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /* Function: iterator row_begin(size_t);
   *           const_iterator row_begin(size_t) const;
   * Function: iterator row_end(size_t);
   *           const_iterator row_end(size_t) const;
   * Usage: for (Matrix<3, 3>::iterator itr = m.row_begin(0); 
   *             itr != m.row_end(0); ++itr)
   * -----------------------------------------------------------------
   * Returns iterators delineating a particular row of the Matrix.  No
   * bounds-checking is performed.
   */
  iterator row_begin(size_t row);
  iterator row_end(size_t row);
  const_iterator row_begin(size_t row) const;
  const_iterator row_end(size_t row) const;

  /* Compound addition and subtraction. */
  Matrix& operator+= (const Matrix& rhs);
  Matrix& operator-= (const Matrix& rhs);
  
  /* Compound scalar multiplication and division. */
  Matrix& operator*= (const ElemType& scalar);
  Matrix& operator/= (const ElemType& scalar);

private:
  ElemType elems[Rows * Cols];
};

/* Binary addition and subtraction. */
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator+ (const Matrix<M, N, T>& lhs, 
                                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator- (const Matrix<M, N, T>& lhs, 
                                 const Matrix<M, N, T>& rhs);

/* Binary scalar multiplication and division. */  
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator* (const Matrix<M, N, T>& lhs, 
                                 const T& scalar);
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator* (const T& scalar, 
                                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator/ (const Matrix<M, N, T>& lhs, 
                                 const T& scalar);

/* Unary + and - */
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator+ (const Matrix<M, N, T>& operand);
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator- (const Matrix<M, N, T>& operand);

/* Binary multiplication. */
template <size_t M, size_t N, size_t P, typename T>
const Matrix<M, P, T> operator*(const Matrix<M, N, T>& lhs, 
                                const Matrix<N, P, T>& rhs);
  
/* Compound multiplication (square matrices only) */
template <size_t M, typename T>
Matrix<M, M, T>& operator*= (Matrix<M, M, T>& lhs, 
                             const Matrix<M, M, T>& rhs);

/* Comparison operators */
template <size_t M, size_t N, typename T>
bool operator== (const Matrix<M, N, T>& lhs,
                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
bool operator!= (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
bool operator<  (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
bool operator<= (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
bool operator>= (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs);
template <size_t M, size_t N, typename T>
bool operator>  (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs);
  
/* Auxiliary Helpers */
template <size_t M, typename T> 
Matrix<M, M, T> Identity();
template <size_t M, size_t N, typename T>
const Matrix<N, M, T> Transpose(const Matrix<M, N, T>& m);

/* Type: Vector
 * ---------------------------------------------------------------------
 * A type representing a row vector in n-dimensional space.  This class
 * is provided as a convenient alternative to using an n x 1 matrix class.
 */
template <size_t Rows, typename ElemType = double>
class Vector {
public:
  /* Constructor: Vector()
   * Usage: Vector<3> v;
   * -----------------------------------------------------------------
   * Constructs a new vector of the requisite dimension.  Elements are
   * not initialized, so make sure that you set the Vector to a reasonable
   * default.
   */
  Vector();

  /* Constructor: Vector(InputIterator begin, InputIterator end);
   * Usage: Vector<3> vec(v.begin(), v.end())
   * -----------------------------------------------------------------
   * Constructs a vector out of a range of values specified by an iterator
   * range.  The behavior is undefined if the range does not have exactly
   * the right number of elements.
   */
  template <typename InputIterator> 
  Vector(InputIterator begin, InputIterator end);

  /* Function: ElemType& at(size_t row, size_t col)
   *           const ElemType& at(size_t row, size_t col) const;
   * Usage: v.at(2) = 137.0;
   * -----------------------------------------------------------------
   * Retrieves a single element in the Vector.  No bounds-checking is
   * performed.
   */
  ElemType& at(size_t row);
  const ElemType& at(size_t row) const;
  
  /* Function: ElemType& operator[](size_t row);
   *           const ElemType& operator[](size_t row);
   * Usage: v[0] = 137.0;
   * -----------------------------------------------------------------
   * Implementation of array element selection.  This allows you to treat
   * the Vector as through it were a raw array.
   */
  ElemType& operator[] (size_t row);
  const ElemType& operator[] (size_t row) const;

  /* Function: size_t numRows() const;
   * Function: size_t numCols() const;
   * Usage: for (size_t i = 0; i < m.numRows(); ++i)
   * -----------------------------------------------------------------
   * Returns the number of rows or columns in the Vector.  The number
   * of rows is specified as a template argument, while the number of
   * columns is always one.
   */
  size_t numRows() const;
  size_t numCols() const;
  
  /* Function: size_t size() const;
   * Usage: std::fill_n(v.begin(), v.size(), 0.0);
   * -----------------------------------------------------------------
   * Returns the total number of elements in the Vector.  This is equal
   * to numRows() and is provided to facilitate code to treat Vectors
   * and Matrices uniformly.
   */
  size_t size() const;

  /* Type: iterator
   * Type: const_iterator
   * -----------------------------------------------------------------
   * STL-compatible iterator and const_iterator types.  These iterators
   * traverse the Vector elements one at a time.
   */
  typedef ElemType* iterator;
  typedef const ElemType* const_iterator;

  /* Function: iterator begin();
   *           const_iterator begin() const;
   * Function: iterator end();
   *           const_iterator end() const;
   * Usage: for (Vector<3>::iterator itr = v.begin(); itr != v.end(); ++itr)
   * -----------------------------------------------------------------
   * Returns iterators delineating the entire contents of the Vector.
   */
  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /* Function: iterator row_begin(size_t);
   *           const_iterator row_begin(size_t) const;
   * Function: iterator row_end(size_t);
   *           const_iterator row_end(size_t) const;
   * Usage: for (Vector<3>::iterator itr = m.row_begin(0); 
   *             itr != m.row_end(0); ++itr)
   * -----------------------------------------------------------------
   * Returns iterators delineating a single element of the Vector.  This
   * is provided primarily to allow Vectors and Matrices to be treated
   * uniformly in template code.
   */
  iterator row_begin(size_t row);
  iterator row_end(size_t row);
  const_iterator row_begin(size_t row) const;
  const_iterator row_end(size_t row) const;

  /* Definition of compound assignment operators. */
  Vector& operator += (const Vector& rhs);
  Vector& operator -= (const Vector& rhs);
  Vector& operator *= (const ElemType& rhs);
  Vector& operator /= (const ElemType& rhs);

private:
  ElemType elems[Rows];
};

/* Binary addition and subtraction. */
template <size_t M, typename T>
const Vector<M, T> operator+ (const Vector<M, T>& lhs, 
                              const Vector<M, T>& rhs);
template <size_t M, typename T>
const Vector<M, T> operator- (const Vector<M, T>& lhs, 
                              const Vector<M, T>& rhs);

/* Binary scalar multiplication and division. */  
template <size_t M, typename T>
const Vector<M, T> operator* (const Vector<M, T>& lhs, 
                              const T& rhs);
template <size_t M, typename T>
const Vector<M, T> operator* (const T& lhs, 
                              const Vector<M, T>& rhs);
template <size_t M, typename T>
const Vector<M, T> operator/ (const Vector<M, T>& lhs, 
                              const T& rhs);

/* Unary + and - */
template <size_t M, typename T>
const Vector<M, T> operator+ (const Vector<M, T>& operand);
template <size_t M, typename T>
const Vector<M, T> operator- (const Vector<M, T>& operand);

/* Matrix multiplication. */
template <size_t M, size_t N, typename T>
const Vector<M, T> operator*(const Matrix<M, N, T>& lhs, 
                             const Vector<N, T>& rhs);

/* Comparison operators */
template <size_t M, typename T>
bool operator== (const Vector<M, T>& lhs, 
                 const Vector<M, T>& rhs);
template <size_t M, typename T>
bool operator!= (const Vector<M, T>& lhs, 
                 const Vector<M, T>& rhs);
template <size_t M, typename T>
bool operator<  (const Vector<M, T>& lhs,
                 const Vector<M, T>& rhs);
template <size_t M, typename T>
bool operator<= (const Vector<M, T>& lhs, 
                 const Vector<M, T>& rhs);
template <size_t M, typename T>
bool operator>= (const Vector<M, T>& lhs, 
                 const Vector<M, T>& rhs);
template <size_t M, typename T>
bool operator>  (const Vector<M, T>& lhs, 
                 const Vector<M, T>& rhs);

/* Stream insertion. */
template <size_t M, typename T, typename charT, typename traits>
std::basic_ostream<charT, traits>& 
operator<< (std::basic_ostream<charT, traits>&, const Vector<M, T>&);

/* Auxiliary helpers */
template <size_t M, typename T>
T DotProduct(const Vector<M, T>& lhs, 
             const Vector<M, T>& rhs);
template <size_t M, typename T>
const Vector<3, T> CrossProduct(const Vector<3, T>& lhs, 
                                const Vector<3, T>& rhs);
template <size_t M, typename T>
T NormSquared(const Vector<M, T>& v);
template <size_t M, typename T>
T Norm(const Vector<M, T>& v);

/******************* Implementation Below This Point ******************/

/* Implementation Headers */
#include <algorithm>
#include <functional>
#include <numeric>
#include <sstream>
#include <cmath>

/********** Matrix Implementation ************/

/* Default constructor doesn't need to do anything. */  
template <size_t M, size_t N, typename T>
Matrix<M, N, T>::Matrix() {
  // Empty
}

/* Iterator constructor just does a straight copy. */ 
template <size_t M, size_t N, typename T>
template <typename InputIterator>
Matrix<M, N, T>::Matrix(InputIterator rangeBegin, InputIterator rangeEnd) {
  std::copy(rangeBegin, rangeEnd, begin());
}

/* Element access uses row-major order to look up the proper element. */
template <size_t M, size_t N, typename T>
const T& Matrix<M, N, T>::at(size_t row, size_t col) const {
  return *(begin() + row * numCols() + col);
}

template <size_t M, size_t N, typename T>
T& Matrix<M, N, T>::at(size_t row, size_t col) {
  /* This uses the const_cast/static_cast trick to implement the non-const at
   * in terms of the const at.  For more information on this trick, see Scott
   * Meyers' "Effective C++."
   */
  return const_cast<T&>(static_cast<const Matrix<M, N, T>*>(this)->at(row, col));
}

/* Sizing functions just return the appropriate template parameter. */
template <size_t M, size_t N, typename T>
size_t Matrix<M, N, T>::numRows() const {
  return M;
}
template <size_t M, size_t N, typename T>
size_t Matrix<M, N, T>::numCols() const {
  return N;
}
template <size_t M, size_t N, typename T>
size_t Matrix<M, N, T>::size() const {
  return numRows() * numCols();
}

/* Iterators return pointers to the proper positions in the Matrix. */
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::iterator Matrix<M, N, T>::begin() {
  return elems;
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::const_iterator Matrix<M, N, T>::begin() const {
  return elems;
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::iterator Matrix<M, N, T>::end() {
  return begin() + size();
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::const_iterator Matrix<M, N, T>::end() const {
  return begin() + size();
}

/* Row iterators work by just skipping the correct number of elements. */
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::iterator Matrix<M, N, T>::row_begin(size_t row) {
  return begin() + row * numCols();
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::const_iterator Matrix<M, N, T>::row_begin(size_t row) const {
  return begin() + row * numCols();
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::iterator Matrix<M, N, T>::row_end(size_t row) {
  return row_begin(row) + numCols();
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::const_iterator Matrix<M, N, T>::row_end(size_t row) const {
  return row_begin(row) + numCols();
}

/* Square brackets operator implementation.  The MutableReference and 
 * ImmutableReference classes are proxy objects to facilitate the 
 * matrix[row][col] notation.
 */
template <size_t M, size_t N, typename T>
class Matrix<M, N, T>::MutableReference {
public:
  T& operator[](size_t col) {
    return parent->at(row, col);
  }

private:
  /* Private constructor is the only way to make an instance of this type.
   * The Matrix is allowed to access it.
   */
  MutableReference(Matrix* owner, size_t row) : parent(owner), row(row) {}
  friend class Matrix;

  Matrix* const parent;
  const size_t row;
};

template <size_t M, size_t N, typename T>
class Matrix<M, N, T>::ImmutableReference {
public:
  const T& operator[](size_t col) const {
    return parent->at(row, col);
  }

private:
  /* Private constructor is the only way to make an instance of this type.
   * The Matrix is allowed to access it.
   */
  ImmutableReference(const Matrix* owner, size_t row) : parent(owner), row(row) {}
  friend class Matrix;

  const Matrix* const parent;
  const size_t row;
};

/* Actual implementations of operator[] just build and return (Im)mutableReferences */
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::MutableReference Matrix<M, N, T>::operator[](size_t row) {
  return MutableReference(this, row);
}
template <size_t M, size_t N, typename T>
typename Matrix<M, N, T>::ImmutableReference Matrix<M, N, T>::operator[](size_t row) const {
  return ImmutableReference(this, row);
}

/* Compound assignment operators implemented in terms of transform and the 
 * appropriate operator function.
 */
template <size_t M, size_t N, typename T>
Matrix<M, N, T>& Matrix<M, N, T>::operator+= (const Matrix<M, N, T>& rhs) {
  std::transform(begin(), end(),  // First input range is lhs
                 rhs.begin(),     // Start of second input range is rhs
                 begin(),         // Overwrite lhs
                 std::plus<T>()); // Using addition
  return *this;
}

template <size_t M, size_t N, typename T>
Matrix<M, N, T>& Matrix<M, N, T>::operator-= (const Matrix<M, N, T>& rhs) {
  std::transform(begin(), end(),   // First input range is lhs
                 rhs.begin(),      // Start of second input range is rhs
                 begin(),          // Overwrite lhs
                 std::minus<T>()); // Using subtraction
  return *this;
}

template <size_t M, size_t N, typename T>
Matrix<M, N, T>& Matrix<M, N, T>::operator*= (const T& scalar) {
  std::transform(begin(), end(), // Input range is lhs
                 begin(),        // Output overwrites lhs
                 std::bind2nd(std::multiplies<T>(), scalar)); // Scalar mult.
  return *this;
}

template <size_t M, size_t N, typename T>
Matrix<M, N, T>& Matrix<M, N, T>::operator/= (const T& scalar) {
  std::transform(begin(), end(), // Input range is lhs
                 begin(),        // Output overwrites lhs
                 std::bind2nd(std::divides<T>(), scalar)); // Divide by scalar
  return *this;
}

/* Binary operators implemented by compound assignment operators. */
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator+ (const Matrix<M, N, T>& lhs, 
                                 const Matrix<M, N, T>& rhs) {
  return Matrix<M, N, T>(lhs) += rhs;
}
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator- (const Matrix<M, N, T>& lhs, 
                                 const Matrix<M, N, T>& rhs) {
  return Matrix<M, N, T>(lhs) -= rhs;
}
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator* (const Matrix<M, N, T>& lhs, 
                                 const T& scalar) {
  return Matrix<M, N, T>(lhs) *= scalar;
}
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator* (const T& scalar, 
                                 const Matrix<M, N, T>& rhs) {
  return Matrix<M, N, T>(rhs) *= scalar;
}
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator/ (const Matrix<M, N, T>& lhs, 
                                 const T& scalar) {
  return Matrix<M, N, T>(lhs) /= scalar;
}

/* Unary + just returns a copy of its argument. */
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator+ (const Matrix<M, N, T>& operand) {
  return operand;
}

/* Unary minus negates its argument. */
template <size_t M, size_t N, typename T>
const Matrix<M, N, T> operator- (const Matrix<M, N, T>& operand) {
  return Matrix<M, N, T>(operand) *= T(-1);
}

/* Matrix multiplication uses the naive algorithm. */
template <size_t M, size_t N, size_t P, typename T>
const Matrix<M, P, T> operator*(const Matrix<M, N, T>& one, 
                                const Matrix<N, P, T>& two) {
  /* Create a result matrix of the right size and initialize it to zero. */
  Matrix<M, P, T> result;
  std::fill(result.begin(), result.end(), T(0));

  /* Now go fill it in. */
  for (size_t row = 0; row < result.numRows(); ++row)
    for (size_t col = 0; col < result.numCols(); ++col)
      for (size_t i = 0; i < N; ++i)
        result[row][col] += one[row][i] * two[i][col];

  return result;
}

/* Special-case: Multiplication with assignment. */
template <size_t M, typename T>
Matrix<M, M, T>& operator*= (Matrix<M, M, T>& lhs, 
                             const Matrix<M, M, T>& rhs) {
  return lhs = lhs * rhs; // Nothing fancy here.
}

/* Two matrices are equal if they have equal values. */
template <size_t M, size_t N, typename T>
bool operator== (const Matrix<M, N, T>& lhs,
                 const Matrix<M, N, T>& rhs) {
  return std::equal(lhs.begin(), lhs.end(), rhs.begin());
}
template <size_t M, size_t N, typename T>
bool operator!= (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs) {
  return !(lhs == rhs);
}

/* The less-than operator uses the std::mismatch algorithm to chase down
 * the first element that differs in the two matrices, then returns whether
 * the lhs element is less than the rhs element.  This is essentially a
 * lexicographical comparison optimized on the assumption that the two
 * sequences have the same size.
 */
template <size_t M, size_t N, typename T>
bool operator<  (const Matrix<M, N, T>& lhs, 
                 const Matrix<M, N, T>& rhs) {
  /* Compute the mismatch. */
  std::pair<typename Matrix<M, N, T>::const_iterator,
            typename Matrix<M, N, T>::const_iterator> disagreement =
    std::mismatch(lhs.begin(), lhs.end(), rhs.begin());
  
  /* lhs < rhs only if there is a mismatch and the lhs's element is
   * lower than the rhs's element.
   */
  return disagreement.first != lhs.end() &&
         *disagreement.first < *disagreement.second;
}

/* The remaining relational operators are implemented in terms of <. */
template <size_t M, size_t N, typename T>
  bool operator<= (const Matrix<M, N, T>& lhs, const Matrix<M, N, T>& rhs)
{
  /* x <= y  iff  !(x > y)  iff  !(y < x) */
  return !(rhs < lhs);
}
template <size_t M, size_t N, typename T>
  bool operator>= (const Matrix<M, N, T>& lhs, const Matrix<M, N, T>& rhs)
{
  /* x >= y  iff  !(y > x)  iff  !(x < y) */
  return !(lhs < rhs);
}
template <size_t M, size_t N, typename T>
  bool operator>  (const Matrix<M, N, T>& lhs, const Matrix<M, N, T>& rhs)
{
  /* x > y  iff  y < x */
  return !(rhs < lhs);
}

/* Transposition is reasonably straightforward. */
template <size_t M, size_t N, typename T>
  const Matrix<N, M, T> Transpose(const Matrix<M, N, T>& m)
{
  Matrix<N, M, T> result;
  for (size_t row = 0; row < m.numRows(); ++row)
    for (size_t col = 0; col < m.numCols(); ++col)
      result[col][row] = m[row][col];
  return result;
}

/* Identity matrix just fills in the diagonal. */
template <size_t M, typename T> Matrix<M, M, T> Identity()
{
  Matrix<M, M, T> result;
  for (size_t row = 0; row < result.numRows(); ++row)
    for (size_t col = 0; col < result.numCols(); ++col)
      result[row][col] = (row == col ? T(1) : T(0));
  return result;
}

/********** Vector Implementation ************/

/* Vector constructor does nothing. */
template <size_t M, typename T>
  Vector<M, T>::Vector()
{
}

/* Iterator constructor just copies data. */
template <size_t M, typename T>
  template <typename InputIterator>
    Vector<M, T>::Vector(InputIterator rangeBegin, InputIterator rangeEnd)
{
  std::copy(rangeBegin, rangeEnd, begin());
}

/* Element access implemented using iterators. */
template <size_t M, typename T>
  T& Vector<M, T>::at(size_t row)
{
  return const_cast<T&>(static_cast<const Vector<M, T>*>(this)->at(row));
}
template <size_t M, typename T>
  const T& Vector<M, T>::at(size_t row) const
{
  return *(begin() + row);
}
template <size_t M, typename T>
  T& Vector<M, T>::operator[](size_t row)
{
  return const_cast<T&>(static_cast<const Vector<M, T>&>(*this)[row]);
}
template <size_t M, typename T>
  const T& Vector<M, T>::operator[](size_t row) const
{
  return at(row);
}

/* Size functions are mostly straightforward. */
template <size_t M, typename T>
  size_t Vector<M, T>::numRows() const
{
  return M;
}
template <size_t M, typename T>
  size_t Vector<M, T>::numCols() const
{
  return 1;
}
template <size_t M, typename T>
  size_t Vector<M, T>::size() const
{
  return numRows() * numCols();
}

/* Iterator functions just return iterators to the corresponding elements
 * of the Vector.
 */
template <size_t M, typename T>
  typename Vector<M, T>::iterator Vector<M, T>::begin()
{
  return elems;
}
template <size_t M, typename T>
  typename Vector<M, T>::const_iterator Vector<M, T>::begin() const
{
  return elems;
}
template <size_t M, typename T>
  typename Vector<M, T>::iterator Vector<M, T>::end()
{
  return begin() + size();
}
template <size_t M, typename T>
  typename Vector<M, T>::const_iterator Vector<M, T>::end() const
{
  return begin() + size();
}

/* Row iterators just delineate a single element. */
template <size_t M, typename T>
  typename Vector<M, T>::iterator Vector<M, T>::row_begin(size_t row)
{
  return begin() + row;
}
template <size_t M, typename T>
  typename Vector<M, T>::const_iterator Vector<M, T>::row_begin(size_t row) const
{
  return begin() + row;
}
template <size_t M, typename T>
  typename Vector<M, T>::iterator Vector<M, T>::row_end(size_t row)
{
  return row_begin(row) + 1;
}
template <size_t M, typename T>
  typename Vector<M, T>::const_iterator Vector<M, T>::row_end(size_t row) const
{
  return row_begin(row) + row + 1;
}

/* Compound assignment operator etc. similar to the Matrix implementations. */
template <size_t M, typename T>
  Vector<M, T>& Vector<M, T>::operator+= (const Vector<M, T>& rhs)
{
  std::transform(begin(), end(),  // All of the receiver object
                 rhs.begin(),     // All of the second vector
                 begin(),         // Overwrite receiver
                 std::plus<T>()); // Addition
  return *this;
}
template <size_t M, typename T>
  Vector<M, T>& Vector<M, T>::operator-= (const Vector<M, T>& rhs)
{
  std::transform(begin(), end(),   // All of the receiver object
                 rhs.begin(),      // All of the second vector
                 begin(),          // Overwrite receiver
                 std::minus<T>()); // Subtraction
  return *this;
}

template <size_t M, typename T>
  Vector<M, T>& Vector<M, T>::operator*= (const T& rhs)
{
  std::transform(begin(), end(),                           // Transform vector
                 begin(),                                  // Overwrite vector
                 std::bind2nd(std::multiplies<T>(), rhs)); // Scale by rhs
  return *this;
}

template <size_t M, typename T>
  Vector<M, T>& Vector<M, T>::operator/= (const T& rhs)
{
  std::transform(begin(), end(),                        // Transform vector
                 begin(),                               // Overwrite vector
                 std::bind2nd(std::divides<T>(), rhs)); // Divide by rhs
  return *this;
}

/* Binary operators implemented in terms of the compound assignment operators. */
template <size_t M, typename T>
  const Vector<M, T> operator+ (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  return Vector<M, T>(lhs) += rhs;
}
template <size_t M, typename T>
  const Vector<M, T> operator- (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  return Vector<M, T>(lhs) -= rhs;
}
template <size_t M, typename T>
  const Vector<M, T> operator* (const Vector<M, T>& lhs, const T& rhs)
{
  return Vector<M, T>(lhs) *= rhs;
}
template <size_t M, typename T>
  const Vector<M, T> operator* (const T& lhs, const Vector<M, T>& rhs)
{
  return Vector<M, T>(rhs) *= lhs;
}
template <size_t M, typename T>
  const Vector<M, T> operator/ (const Vector<M, T>& lhs, const T& rhs)
{
  return Vector<M, T>(lhs) /= rhs;
}

/* Unary + just returns a copy of the argument */
template <size_t M, typename T>
  const Vector<M, T> operator+ (const Vector<M, T>& operand)
{
  return operand;
}

/* Unary - implemented in terms of *= */
template <size_t M, typename T>
  const Vector<M, T> operator- (const Vector<M, T>& operand)
{
  return Vector<M, T>(operand) *= T(-1);
}

/* Multiplication with a matrix is actually fairly efficient due to the way that
 * the elements are guaranteed to be layed out in memory.
 */
template <size_t M, size_t N, typename T>
  const Vector<M, T> operator*(const Matrix<M, N, T>& lhs, const Vector<N, T>& rhs)
{
  Vector<M, T> result;

  /* Set each component to the inner product of the proper row of the matrix
   * and the entire vector.
   */
  for (size_t row = 0; row < lhs.numRows(); ++row)
    result[row] = std::inner_product(lhs.row_begin(row), lhs.row_end(row), // Proper row of the matrix
                                     rhs.begin(), // All of the vector
                                     T(0));       // Start counting at zero.

  return result;
}

/* Comparison operators layered on std::equal */
template <size_t M, typename T>
  bool operator== (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  return std::equal(lhs.begin(), lhs.end(), // All of left vector
                    rhs.begin());           // All of right vector
}
/* Disequality implemented in terms of equality. */
template <size_t M, typename T>
  bool operator!= (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  return !(lhs == rhs);
}
/* The less-than operator uses the std::mismatch algorithm to chase down
 * the first element that differs in the two matrices, then returns whether
 * the lhs element is less than the rhs element.  This is essentially a
 * lexicographical comparison optimized on the assumption that the two
 * sequences have the same size.
 */
template <size_t M, typename T>
  bool operator<  (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  /* Compute the mismatch. */
  std::pair<typename Vector<M, T>::const_iterator,
            typename Vector<M, T>::const_iterator> disagreement =
    std::mismatch(lhs.begin(), lhs.end(), rhs.begin());
  
  /* lhs < rhs only if there is a mismatch and the lhs's element is
   * lower than the rhs's element.
   */
  return disagreement.first != lhs.end() &&
         *disagreement.first < *disagreement.second;
}

/* The remaining relational operators are implemented in terms of <. */
template <size_t M, typename T>
  bool operator<= (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  /* x <= y  iff  !(x > y)  iff  !(y < x) */
  return !(rhs < lhs);
}
template <size_t M, typename T>
  bool operator>= (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  /* x >= y  iff  !(y > x)  iff  !(x < y) */
  return !(lhs < rhs);
}
template <size_t M, typename T>
  bool operator>  (const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  /* x > y  iff  y < x */
  return !(rhs < lhs);
}

/* Stream insertion buffers the result to a stringstream and inserts the entire
 * result at once.
 */
template <size_t M, typename T, typename charT, typename traits>
  std::basic_ostream<charT, traits>& operator<< (std::basic_ostream<charT, traits>& out,
                           const Vector<M, T>& v)
{
  /* Build the stringstream to hold the result. */
  std::basic_stringstream<charT, traits> buffer;
  
  /* Copy formatting info. */
  buffer.imbue(out.getloc());
  buffer.flags(out.flags());
  buffer.precision(out.precision());

  /* Get a copy of the code conversion facet. */
  const std::ctype<charT>& ccvt = std::use_facet< std::ctype<charT> >(out.getloc());

  /* Print out the vector. */
  for (size_t i = 0; i < v.numRows(); ++i)
    buffer << v[i] << ccvt.widen(' ');
  
  /* Write the buffer to the stream and return. */
  return out << buffer.str();
}

/* Dot product implemented in terms of inner_product. */
template <size_t M, typename T>
  T DotProduct(const Vector<M, T>& lhs, const Vector<M, T>& rhs)
{
  return std::inner_product(lhs.begin(), lhs.end(), // First vector's elems
                            rhs.begin(),            // Second vector's elems
                            T(0));                  // Counting from zero
}

/* Norms implemented in terms of the dot product. */
template <size_t M, typename T>
  T NormSquared(const Vector<M, T>& v)
{
  return DotProduct(v, v);
}
template <size_t M, typename T>
  T Norm(const Vector<M, T>& v)
{
  return std::sqrt(NormSquared(v));
}

template <typename T>
  const Vector<3, T> CrossProduct(const Vector<3, T>& u, const Vector<3, T>& v)
{
  /* The cross-product of two vectors u and v in 3-space is defined as
   * |i  j  k |
   * |u0 u1 u2| = i |u1 u2| - j |u0 u2| + k |u0 u1|
   * |v0 v1 v2|     |v1 v2|     |v0 v2|     |v0 v1|
   *
   *            = i (u1 v2 - u2 v1) - j (u0 v2 - u2 v0) + k (u0 v1 - u1 v0)
   *            = i (u1 v2 - u2 v1) + j (u2 v0 - u0 v2) + k (u0 v1 - u1 v0)
   *            = (u1 v2 - u2 v1, u2 v0 - u0 v2, u0 v1 - u1 v0)
   */
  Vector<3, T> result;
  result[0] = u[1] * v[2] - u[2] * v[1];
  result[1] = u[2] * v[0] - u[0] * v[2];
  result[2] = u[0] * v[1] - u[1] * v[0];
  return result;
}

#endif
