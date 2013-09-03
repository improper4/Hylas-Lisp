# Lambda Functions and their Implementation

Essentially, whenever the compiler looks up a symbol, it asks "Are we inside a lambda context?" and "Does this symbol come from outside of this context?". If both are true, then this symbol is added to the list of symbols in the latest context of the `lambda-context` slot of the `code` object. When the lambda is to be emitted, these 'external' symbols become arguments to the lambda, which LLVM's trampolining intrinstics 'excise out', finally delivering a pointer to the closure.
