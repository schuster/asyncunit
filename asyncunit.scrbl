#lang scribble/manual

@title{AsyncUnit}

AsyncUnit is an extension to RackUnit with forms for testing communication over asynchronous channels.

@defform[(check-unicast chan v #:description description #:timeout timeout)]{
  Checks that @racket[chan] produces the message @racket[v] within @racket[timeout] seconds, using @racket[description] as a description of the expected message.
}

@defform[(check-unicast-match chan pat pred #:timeout timeout)]{
  Checks that @racket[chan] produces a message that matches the pattern @racket[pat] within @racket[timeout] seconds. @racket[pred] acts like it does for @racket[check-match].(@racket[pred] and @racket[timeout] are optional, but I need to figure out how to show that in Scribble.)
}
