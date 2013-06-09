/*************************************************************************
 * File: function.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * A class that can hold a reference to any unary function, whether that
 * function is a true C++ function or a functor (a class overloading
 * operator()).  The class is implemented using a trick called external
 * polymorphism.  Internally, the class stores a pointer to a polymorphic
 * class that exports a function called execute.  When the Function is
 * assigned a value, it wraps that value up in a wrapper subclass of
 * the polymorphic class.  In this way, the Function object has type-safe
 * access to the function, since no matter what the actual type of the
 * function is, Function always sees it through the polymorphic wrapper.
 */

#ifndef Function_Included
#define Function_Included

/* This type is parameterized over its argument and return types.  It is
 * expected that one will treat these types as if one were writing a true
 * function, so the argument type should be a reference-to-const where
 * appropriate, etc.  For example, one could write
 *
 * Function<int, void> f = MyFunction;
 *
 * This would declare an object called f that takes in an int, returns
 * void, and which refers to a function MyFunction.  If we call f, as
 * shown here:
 *
 * f(137);
 *
 * This will then call MyFunction, passing in 137.
 */
template <typename ArgType, typename ReturnType>
class Function {
public:
  /* Constructor: Function(UnaryFunction function);
   * Usage: Function<int, int> myFunction = SomeFunction;
   * ----------------------------------------------------------------------
   * Constructs a new Function object that wraps the specified unary
   * function.  It is assumed that the function can be called with ArgType
   * as its argument type and ReturnType as its return type; if this is
   * not the case, the program will fail to compile.
   */
  template <typename UnaryFunction> Function(UnaryFunction);

  /* Destructor: ~Function();
   * Usage: (implicit)
   * ----------------------------------------------------------------------
   * Deallocates all memory allocated by this Function object.
   */
  ~Function();

  /* Copy functions: Function(const Function& other);
   *                 Function& operator= (const Function& other);
   * ----------------------------------------------------------------------
   * Creates a new Function object that is a copy of an existing Function,
   * or overwrites this Function object with a copy of some other Function.
   */
  Function(const Function& other);
  Function& operator= (const Function& other);

  /* Function call operator: ReturnType operator() (ArgType value) const;
   * Usage: f(137);
   * ----------------------------------------------------------------------
   * Invokes the function wrapped by this Function object, passing in the
   * specified parameter and returning the result of the call.
   */
  ReturnType operator() (ArgType value) const;

private:
  /* Base class which represents some function that can be called with an ArgType
   * that returns a ReturnType. ArbitraryFunctions can also be deep-copied using
   * the clone() function.
   *
   * ArbitraryFunction is an abstract class since there is no good default
   * implementation for any of its member functions.
   */
  class ArbitraryFunction {
  public:
    /* Polymorphic classes need virtual destructors. */
    virtual ~ArbitraryFunction() {}

    /* execute calls the stored function and returns its return value. */
    virtual ReturnType execute(ArgType param) const = 0;

    /* clone returns a deep-copy of the receiver object. */
    virtual ArbitraryFunction* clone() const = 0;
  };

  /* For any type of unary function, we define a subclass of ArbitraryFunction
   * which wraps that object so it can be called through the ArbitraryFunction
   * interface.
   */
  template <typename UnaryFunction>
  class SpecificFunction: public ArbitraryFunction {
  public:
    /* Constructor accepts and stores a UnaryFunction. */
    explicit SpecificFunction(UnaryFunction fn) : function(fn) {}

    /* execute just calls down to the function. */
    virtual ReturnType execute(ArgType param) const {
      return function(param);
    }

    /* Clone returns a deep-copy of this object. */
    virtual ArbitraryFunction* clone() const {
      return new SpecificFunction(*this);
    }
  private:
    /* The actual function that gets called. */
    UnaryFunction function;
  };

  /* This pointer to an ArbitraryFunction is the real implementation of the Function
   * class.  Whatever function clients provide to Function is stored in a wrapped
   * form here.
   */
  ArbitraryFunction* function;

  /* Utility functions used to implement the copy functions.  Clear deallocates all
   * resources allocated by this object; copyOther makes this object a deep-copy of
   * some other object.
   */
  void clear();
  void copyOther(const Function& other);
};

/* Constructor accepts a UnaryFunction of the proper type, then wraps it inside a
 * SpecificFunction wrapper. Note that there are two template headers here since
 * the class and constructor are both templates.
 */
template <typename ArgType, typename ReturnType>
template <typename UnaryFunction>
Function<ArgType, ReturnType>::Function(UnaryFunction fn) {
  function = new SpecificFunction<UnaryFunction>(fn);
}
    
/* Destructor calls clear. */
template <typename ArgType, typename ReturnType>
Function<ArgType, ReturnType>::~Function() {
  clear();
}

/* Copy ctor calls copyOther. */
template <typename ArgType, typename ReturnType>
Function<ArgType, ReturnType>::Function(const Function& other) {
  copyOther(other);
}

/* Standard assignment operator. */
template <typename ArgType, typename ReturnType>
Function<ArgType, ReturnType>&
Function<ArgType, ReturnType>::operator=(const Function& other) {
  if(this != &other) {
    clear();
    copyOther(other);
  }
  return *this;
}

/* clear deletes the stored pointer. */
template <typename ArgType, typename ReturnType>
void Function<ArgType, ReturnType>::clear() {
  delete function;
}

/* copyOther uses the clone() member function to do the copy. Note that the copy
 * is necessary instead of using a shallow copy because the function might be a
 * functor with internal state.
 */
template <typename ArgType, typename ReturnType>
void Function<ArgType, ReturnType>::copyOther(const Function& other) {
  function = other.function->clone();
}

/* Finally, operator() just calls down into the function and returns the result. */
template <typename ArgType, typename ReturnType>
ReturnType Function<ArgType, ReturnType>::operator()(ArgType param) const {
  return function->execute(param);
}

#endif
