## About Humbler
Humbler is a Tumblr API interface that attempts to make it as little of a pain to use as possible. oAuth login and signing is handled by [North](http://shinmera.github.io/north/).

## How To
Begin by loading Humbler and the North backend you prefer. For the purposes of illustration here we'll use Drakma:

    (ql:quickload '(north-drakma humbler))

Before you can use Tumblr's API you'll need an [application](http://www.tumblr.com/oauth/apps). Copy both the consumer key and secret and plug them into a new client:

    (setf humbler:*client* (make-instance 'humbler:client :key api-key :secret api-secret))

Humbler should configure the proper oAuth URLs automatically. All you need to do now is to initiate the authentication by using Login.

    (humbler:login)

This should return an URL if successful. Open this URL in your browser and allow the application access. It should redirect you to a localhost address, which then completes the authentication process automatically. If you want to handle the handshake in your own server, rather than letting Humbler spawn one, pass the `:callback` URL when you create the client instance.

    (humbler:*user*)
    (humbler:name (humbler:*user*))

Humbler offers an object-oriented interface that tries to take a lot of manual work off your hands and a lower-level API-mapping. Here's an example using both to create a text post:

    (humbler:blog/post-text "my-blog" "Hey, testing this Common Lisp library called Humbler..." :title "Whoa nelly!")
    
    (humbler:save (make-instance 'humbler:text-post :body "Nobody reads text posts anyway." :title "Welp."))

While the low-level functions have some limitations and oddities about them (max 20 results on a listing every call, strange inconsistencies, partial objects) due to the way Tumblr's API is made, the high-level functions try to smoothen over these issues so you don't have to worry about them. If for some reason you hate automatisation or how this abstraction works, you are of course free to build your own on top of the bare API calls though.

Humbler's symbols are split into three packages: `HUMBLER-API` which contains the API mapping functions, `HUMBLER-OBJECTS` containing the object and generic function interface, and finally `HUMBLER-EXTRA` with some functions to help deal with Tumblr (only programmatically, not mentally).

## Deferred Slots
Tumblr's API doesn't always return complete objects; some calls only return a slice of the fields an object may exhibit. Humbler tries to take this annoying management off your hands by automatically augmenting your object with slot values from a fitting API call if you try to access it and it is not yet bound. If it fails to augment a slot, either because it does not know any API call to use to retrieve it or because something else went wrong, an error is signalled instead.

## Multiple Users
If you need to allow multiple users simultaneously, you'll need to create a separate `client` instance for each, and make sure to bind `*client*` and `*user*` where you access them.

## More Examples
Post to all your blogs:

    (dolist (blog (my-blogs))
      (repost (make-instance 'text-post :body "Hi!") blog))

Reblog the last 100 photos from a blog:

    (mapcar #'reblog (posts "cool-photo-blog" :type :photo :amount 100))

View the most recent text posts from a tag:

    (mapcar #'body (remove-if-not #'text-post-p (tag "interesting-tag")))

Delete all your drafts:

    (mapcar #'destroy (my-drafts :amount T))

Interactively reply to all your asks:

    (dolist (sub (my-submissions :amount T))
      (when (answer-post-p sub)
        (format T "~a: ~a~%" (asking-name sub) (question sub))
        (reply sub (read-line))))

## Further Reading
* The [symbol index](http://shinmera.github.io/humbler/).
* [North](http://shinmera.github.io/north/), the oAuth library.
* [Chirp](http://shinmera.github.io/chirp/), Humbler's sister library for Twitter.
* Tumblr's frankly abysmal API '[documentation](https://www.tumblr.com/docs/en/api/v2)'.
