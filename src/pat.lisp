;;; # Pattern Matcher
;;; Pattern matching is used to implement parametric polymorphism in Hylas. For example, a generic function might have the following prototype:
;;; 
;;; ((a i64) (b generic-a) (c (fn bool generic-b generic-b)) (d (pointer generic-c)))
;;; 
;;; Here, only the first argument is specific. The second can be any type, the fourth can be any pointer type (With indirection => 1), and the third and most complex is a pointer to any function that returns a boolean and takes any two arguments that must be of the same type. This kind of complexity in generic argument lists requries a pattern matcher, whose usefulness extends beyond generic functions as it can be used to pattern match fully specialized functions and make the process of calling functions more generic.

(in-package :hylas)
