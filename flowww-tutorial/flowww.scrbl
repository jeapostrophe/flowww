#lang scribble/doc
@(require scribble/manual
          (for-label scheme)
          (for-label web-server/servlet)
          "tutorial-util.ss")

@title{@bold{Flowww}: Functional Dataflow Web Applications in PLT Scheme}

@author[(author+email "Jensen Warnock" "jhwarnock@gmail.com")]

After finishing the @link["http://docs.plt-scheme.org/continue/index.html"]{Continue Tutorial}, you should
have a basic understanding of PLT Web server. This tutorial will show 
how you can build functional Web applications that utilize a dataflow 
graph for variable updates using PLT Scheme.  As our working example,
we'll build a simple twitter-like application.  We'll cover how to start
up a functional Web server, how to generate dynamic Web content in a 
dataflow model, and how to interact with the user in this new style.

The target audience for this tutorial are students who've gone through
the design and use of structures in
@italic{@link["http://www.htdp.org/"]{How to Design Programs}}, with
some higher-order functions, @scheme[local], a minor bit of mutation,
and of course the @link["http://docs.plt-scheme.org/continue/index.html"]{Continue Tutorial}.

@section{Getting Started}

Everything you needed in this tutorial is provided in @link["http://plt-scheme.org/"]{PLT Scheme}.
You will be using the DrScheme using the module language with FrTime. 
Enter the following into the Definition window.

@schememod[
           scheme
           (require (planet byu-plt/flowww:1:0))
           (require web-server/servlet-env)
           
           (define ((make-start) request)
             '(html
               (head (title "Fwitter"))
               (body ()
                     (h1 "Under construction"))))
           (serve/servlet (wrap-start (make-start)))
           ]

Press the @onscreen{Run} button.  If a Web browser comes up with an ``Under
Construction'' page, then pat yourself on the back. It doesn't do anything yet, and has 
no actual functional reactivity, but you will get there.  Press the 
@onscreen{Stop} button to shut the server down for now.

@section{The Application}

Through this tutorial you will be shown how to develop a basic
Twitter-esque application. Users should be able to create accounts,
tweet (or fweet), and follow as other change there message. You will
start out with a working Static Fwitter application, and add the reactivity
piece by piece using Flowww.
Along the way, you will see some of the issues faced in building Web apps
and how Flowww addresses them.
By the end of this tutorial, I'll have a simple Twitter-ish application.

@section{Static Fritter}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/control-1)
                                 (web-server/scribblings/tutorial/examples/model-1)
                                 (web-server/scribblings/tutorial/examples/view-1)]

Start by considering the data definitions.  Define a user as a name
and a status. A status will be a single string that changes (no past status
will be available to view once the status changes).

For this example, you will see how Flowww allows programmers
to interact with a non-function persistant data store as though it
is functional. So let's store our users in a hash. You will use the
username as the key to the hash and the staus as the value. You will
also store the users being followed in a hash. the username will be
the key, and the value will be the list of usernames that user follows.

If you would like some practice with concepts related to the
@link["http://docs.plt-scheme.org/continue/index.html"]{Continue Tutorial}
you may write a model the will store user in a hash and 
provide the following functions:
@itemize[
         @item{@scheme[create-user]}
          @item{@scheme[user-status]}
          @item{@scheme[user-status-update!]}
          @item{@scheme[follow-user!]}
          @item{@scheme[user-following]}
          @item{@scheme[user-list]}
          @item{@scheme[user-follow-list]}
          ]
Note: If you don't want or need the practice, the code you will use for the
tutorial will be provided at the end of this section.

Now that you have the ability to add users to our hash. You can add
the HTML view to register a user. Recall PLT Scheme's solution for HTML from the 
@link["http://docs.plt-scheme.org/continue/index.html#%28part._.Rendering_.H.T.M.L%29"]{Rendering HTML}
section of the @link["http://docs.plt-scheme.org/continue/index.html"]{Continue Tutorial}.

HTML in Scheme, Oh my. Another practice opportunity writting two views for your Web App. 
One for creating accounts, and login. The
other view will be to change your status add a user to follow, as well as show
all of the users available to be followed, and the list of users and status
you are currently following.
Note: This is still just for fun and practice, the code is provided below if you want to
jump right in to Flowww.

Now that you have a view and a model.  You need to link their functionality
via a controller.  This file will have a start funciton that calls the 
@scheme[show-login] function. The function @scheme[show-login] will have two handlers, register
and login. The @scheme[reg-handler] will get the username and status from the
request, create a new user in the model with @scheme[create-user], and show the same login page, @scheme[show-login].
The @scheme[login-handler] will take the username from the request, and render the
profile page with the information for that user, @scheme[(show-profile username)].

The show-profile function will take a username, and render the profile view
with the users information.  It will also have two handlers, update status
and follow user. The @scheme[update-handler] will will get the new status 
from the request and update the status in the model calling @scheme[user-status-update!].
The profile page will then be reloaded with the new status. The @scheme[follow-handler]
will pull the 'follow' binding from the request and call the @scheme[follow-user!]
function to update the hash to add that user to the follow list.

Again, if you want the practice, this is a great chance to write some Scheme Web code, and
check your solution with the one provided. Try writing the controller as specified above.

Note: Below is the code referenced throughout the rest of the tutorial.
If you did the exersizes, congratulations for making it through in one piece. You can
either adapting your code so the inferfaces between MVC are similar, or noting the
differences so that the rest of the tutorial makes sense.

Model
@external-file["model-1.ss"]

View
@external-file["view-1.ss"]

Controller
@external-file["control-1.ss"]

Note: In the controller, you'll notice the use of @scheme[binding->string]. This is a helper function provided
by @scheme[flowww-lib].  Someother helper function include @scheme[post-data->string], @scheme[quote-string], and @scheme[escape-string]

@section{Adding Flowww...}
@bold{Not unsimilar to adding Mojo to your Web app.}

Anyone who has done AJAX programming on the Web knows that it can be a pain. Flowww is designed to
ease the suffering. It can be difficult to set up pulling and updating for variables in Web programs.
It can also be just plain boring. So let's see how Flowww helps.

Flowww provides 3 functions and 2 Javascript abstractions to accomplish updating. The functions you will
use are @scheme[func-arg-evt], @scheme[func-arg-bhv], and @scheme[flowww-handler].  You will also use
@scheme[flowww-script] and @scheme[flowww-post] to avoid writing some of the Javascript.

First, you will adapt the model to behave reactively. The first step is to is to add a signal to the 
functions that make a change to the persitant data store, the hash in our case. You will accomplish this
by using @scheme[send-event] provided by FrTime. The first argument will be @scheme[func-arg-evt] from @scheme[flowww-lib], and
the second will be whatever symbol you would like to pass through your event (i.e. @scheme['changed]).
@scheme[func-arg-event] will take the name of the function that the controller calls to get the value
of the data being updated, and the arguments for that function.

Thus...
@schemeblock[
             (define (user-status-update! username status)
               (hash-set! user-ht username status))
                                                   ]
...becomes...
@schemeblock[
             (define (user-status-update! username status)
               (hash-set! user-ht username status)
               (send-event (func-arg-evt user-status username) 'changed))
                                                                         ]

@bold{Exercise.} Identify and adapt all of the functions that update hash tables.

Next, you need to alter the "getter" functions to become behaviors that update as the persistant store
changes. You will use @scheme[func-arg-bhv] to turn the funcitons into behavoirs.

@scheme[(define user-status-b (func-arg-bhv user-status))]

You must then provide the new function for external calls so they only see behavoirs.

@bold{Exercise.} Alter the remainder of the file.

That is all you have to do to the model. Yep that's it. Now let's fix the controller.
You will use the Javascript abstractions in the controller. Let's add them.

First, an overview of what they are.  @scheme[flowww-script] contains the Javascript functions needed
to post data for the AJAX application. @scheme[flowww-post] formats the url in Javascript and calls the
functions in @scheme[floww-script] to post the data from the second argument.

So there are a few things to change. You need to pass the @scheme[flowww-script] to all of the views that need
to post data. For example,
@schemeblock[(render-profile username
                             (user-status username)
                             (make-url update-handler)
                             (make-url follow-handler)
                             (user-list)
                             (user-follow-list username)
                             flowww-script)]

Any handler that you want to be handled as AJAX instead of loading a new page will need
to be updated. You can use this using @scheme[flowww-handler] as follows.
@schemeblock[
             [update-handler
              (lambda (request)
                (define status
                  (binding->string #"status" request))
                (user-status-update! username status)
                (show-profile username))]]
...will be updated to read...
@schemeblock[
             [update-handler
              (flowww-handler
               (lambda (request)
                 (define status
                   (binding->string #"status" request))
                 (user-status-update! username status)))]]

Finally, you will need to change the urls of AJAX requests. @scheme[flowww-post] will help
us make the switch. It will take the url made from the handler, and the data to post. The only
drawback is the function will be called in the HTML using Javascript when the user triggers
the event. So it will have to be written in Javascript using @link["http://planet.plt-scheme.org/package-source/jaymccarthy/javascript.plt/1/2/doc.txt"]{this syntax}.
For example, if you want to get the status value out of the textbox you would write something
similar to the code below.
@schemeblock[(field
              ((field
                document 
                getElementById) 
               "status") 
              value)]

@bold{Exercise.} Finish up the conversion of the controller.

You are almost done. You only have the view left to do. Unfortunately, there is a step
in this process that is more complicated then it should be. In order for the behavoirs
to be updated and render correctly in the HTML, the HTML @scheme[s-exrp] need to be
slightly hacked. Since this is the only difference, it is best explained by showing
the new code and letting you look over it. Essentially, an empty string needs to be
added to the end of lists that contain behaviors. Check out the code below for further
understanding.

Here is the finished view...
@external-file["view-2.ss"]

Here is the final controller...
@external-file["control-2.ss"]

And the resulting model...
@external-file["model-2.ss"]

Flowww provides the tools needed to create the next level of Web Applications. With all
of the AJAX implementation details abstracted away, developers can focus on the essence
of their creations. Their minds can be free to explore sophisticated solutions to new
problems that were never tackled before due to excessive complication of lower level
details. Some individuals with various perspectives and expertise may now be able to put
together a novel application and introduce new innovation that would not have been
possible without the abstraction Flowww provides. We hope that you find it useful, and
invite you to go change the world.