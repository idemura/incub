import http.*  # Import all
import cron:Type,method
import time  # Can access as time.Now()

fn absDist(a I32, b I32) I32  # here I can put or not :
# or
fn absDist(a, b I32) I32
  if a > b:  # : or not, : used for one liners.
    return a - b
  else  # :?
    return b - a

# maybe out var? which goes to the outer scope? Need to be defined in
# all branches and have same type.
if rand() % 2 == 0
  out var rv = 100
else
  out var rv = 200
println("rv = %0", rv)

fn f2(s String) I32
  var c I32
  var a, b I32
  var a = 10, b I32  # initializers must be ok with type declaration.
  # for - in traverses immutable keys.
  for i in s:
    case s[i]
    of `a..`b:
      c += 1
    of `0..`9:
      c -= 1
    # of `\n `\
    # Here I need :
    else: break
  return c

# for cycle:
for key in collection <var/val? too many keywords, value, var? as/as var?> ki
  print(key, ki)

for key in coll
  print(key, coll[key])

# You may need to name (alias) collection:
for key in coll = coll_a ~ coll_b
  print(key, coll[key])

# generic loop is for too?
var i = 0
while i < 100
  # next line in case of { must be not closer than { position!
  if { s[i] % 100 < 50 && subCheck(s[i]) &&
       !anti(s[i]) }
    counter += 1
  # format line: %0 and then optionally : and format spec up to ;.
  # for integers: d, h, o, b - decimal, hex, octal, binary.
  # , may be omitted
  print("%0:x,2,0;", i)
  # with this syntax, 'continue' doesn't make big sense.
  i += rand() % 13
println("")

# ENUM
enum DaysOfWeek
  # Big question about naming!
  # Allow or now capital letter in the beginning? I think no.
  monday  # 0 by default, as C.
  tuesday  # +1 by default,
  wednesday

enum Masks
  # Enums default value is the first in the enum list!
  bit1 = 1  # explicit set
  bit2 = _ * 2  # compile time. _ means previous value as base int value.

# Setting U64 as base integral value.
enum E2(U64)
  val1
  val2

# varargs!?
# overload? syntax like: see below.
fn f1(x) Void
  dispatch x Type:
    <body here>
  dispatch x:  # everything else. We don't have base object

# tuple
# probably short list like x, y I32. What with
# x, y I32, name String
# seems like our philosophy type is tag to the term! So it's not allowed.
# memeber are always same access as tuple type.
# No now we'd better allow it.
record Point(x I32, y I32, s String)
record Point(x, y I32, s String)
# can be in the heap.
record Point
  x, y I32  # rule broken (type to token). The same for var?:
            #   var x, y I32
            #   var x, y = 0, 0 considered harmful as "many ways to do the same"

# Need to have constructors since immutability support.
# what syntax?
fn MyData(Iptr sz)
  this.data = new I32[](sz)
  # don't mix type definition and runtime arguments
  var a2 = new I32[][](10, 20)
  var a2 = new I32[][](10);  # Last index array can be omitted. Empty arrays.
  # arrays are like vectors. Operators or functions?

# CAST
# Core feature - cast show exact type from and to:
x cast(F32, I32)  # Here, x is F32 and casted to I32.
# cast can be treated as binary operator.
x I32
(x + y) cast(F32, I32)
(x cast(Object, MyObject)).left = 12

# If struct implements something, it MUST be allocated in the heap.
# Maybe better way? Pure for that doesn't allow "implement"?
# Value and class + interface.
record MyData
  implements AdderBack, Remover
  data I32[]
  left, right MyData*
  public name, tag String  # public to all the list of name and tag

# non-interface function.
# this or _?
fn MyData.printName()
  print("hello " + this.name)

# can omit all the types for call instantiation.
# this is here
fn MyData.add(a)
  data += a

# a.f(x) is syntactic sugar for f(a, x)!
# if it's syntactic sugar, this automatically means overloading!!!
# consider two classes with the same setTime function!
class A
  empty  # need keyword? like pass or just ;
# syntax implicitly adds first argument 'this A&' or 'this A!I'.
fn A.setTime(t I64) Void
  empty

# immutable object functions...
fn A!I.getTime() I64: return 10

# record - value type. bitwise only copy option. if implements a class,
#   must be allocated in the heap. may have pointer types.
# object - reference type. never bit copy. only ref. only heap. Pointer to
#   it doesn't make sense.
# class - interface

class B
  empty  # Better use pythonic pass as less wtf.
fn B.setTime(t I64) Void
  empty

# it may be the way to have same named functions! this means overloading on
# the first term. may be this is what we can support?
# don't support overloading for free functions! but do for class-like
# syntax implicitly by new var (you can always expand existing class without
# modifying basic source - like, if class defined in a library).
var a = new A, b = new B
a.setTime(10) => setTime(a, 10)  # OVERLOADING!!! otherwise, we don't need it.
b.setTime(20) => setTime(b, 10)

# !!! consider ! or $. Seems like ! is less visual noise.
# consider ' also.
# A$I -- probably lower case?
# A!I
# A'I
# A$i
# A!i
# A'i
# WindowWidget$I
# WindowWidget!I
# WindowWidget'I
# WindowWidget!i
# WindowWidget$i
# WindowWidget'i

# !!! decide about `this` type!!!
# so, overload:
fn I32.printLn()
  print this  # wow! seems like ok for class.
fn String.printLn()
  print this

# {} are free for some literals!

# class extends it's attribute to all functions.  Need private make 2 classes?
# In any case, it's error if out access modifier is weaker (more public) than
# outer.
# Need private keyword...
# Probably make access modifier hierarchical? For all.
public class AdderBack:
  # Type is omitted means auto deduction.
  fn add(a I32) Void  # public by default

# value/reference symantics.
# function parameters passed by best possible way, not modifiable
# if you need to modify, use var:
# Internally type of such var is Type&, and can't be created by the programmer.
# and it can't be stored. Even by copy. Only passed to another var x function.
# Type* means value in the garbage collected heap and reference semantics.
# Value type is Type
# Type[] - array in GC heap, reference
# Type[10] - value semantics, not in heap.
# Probably, need GC to value and versa?
# So, string is actually Char[]
fn increment(var x I32) Void
  x += 1

# usage:
var i1 = -1
increment(&i1)

# module:
# private by default
# Public exported out of file to outer directory and other file from this
# directory and upper can use it.  File can't access public of higher dir,
# because it it's sub module (check: it's convenient to copy it and reuse. If
# allow to access upper, copying isn't possible).
# Export allows to use symbols.

# TEMPLATES!
# if unnamed template, then only for the record/object/fn immediately after.
template(T)
class Stream
  fn read(out T x)
  fn write(T x)

template CopierT(T, In, Out, K)
  In is Stream(T)
  Out is Stream(T)
  K is [I32, I64, I16, I8]
