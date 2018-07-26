#|
 This file is a part of Humbler
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defmacro docf (function docstring &rest format-args)
  `(setf (documentation ',function 'function)
         (format NIL ,docstring ,@format-args)))

(defmacro docc (class docstring &rest format-args)
  `(setf (documentation (find-class ',class) T)
         (format NIL ,docstring ,@format-args)))

;;;
;;; BLOG.LISP
;;;

(docf blog/info
      "Returns information about a blog.

Included fields:
TITLE        --- The title of the blog.
POSTS        --- Total number of posts on this blog.
NAME         --- The blog username.
UPDATED      --- Unix-Time of the last update.
DESCRIPTION  --- The blog's description.
ASK          --- T if asks are enabled, NIL otherwise.
ASK-ANON     --- T if anonymous asks are allowed, NIL otherwise.
                 Only included if ASK is T.
LIKES        --- The number of posts this user has liked.
                 Only included if likes sharing is enabled and this blog
                 is indeed a user.")

(docf blog/avatar
      "Returns the avatar image as a PNG octet-sequence.

SIZE  --- The size of the avatar to return, one of
          16 24 30 40 48 64 96 128 512")

(docf blog/likes
      "Returns a list of publicly shown posts that were liked by this blog.
Secondary value returns the total number of liked posts.

LIMIT   --- The number of posts to include, must range between 1 and 20.
OFFSET  --- The offset in posts to return.

See BLOG/POSTS for information on the structure of a post object.")

(docf blog/followers
      "Returns a list of followers of this blog.
Secondary value returns the total number of followers.

LIMIT   --- The number of posts to include, must range between 1 and 20.
OFFSET  --- The offset in posts to return.

Each object in the list has the following fields:
NAME       --- The user's name.
FOLLOWING  --- T if you are following this user, NIL otherwise.
URL        --- The address to the primary blog of this user.
UPDATED    --- Unix-time of the last update of this user.")

;;;
;;; POST.LISP
;;;

(defun post-docstring (type required optionals)
  (format NIL "Creates a ~a post and returns the post's ID.

Required arguments:
BLOG       --- string
               The name of the blog to post to. Your account has to have access
               to this blog in order for the post to succeed.
~a

Optional arguments:
~a
STATE      ::= :published | :draft | :queue | :private
               Sets the state of the post, can later be changed through BLOG/POST/EDIT.
TAGS       ::= string | list
               Either a list or a comma-separated string of tags.
TWEET      ::= string | :off
               If :off no tweet is made, if a string is given it is used as the tweet text.
DATE       ::= string | local-time:timestamp
               'The GMT date and time of the post' according to doc, but no further
               specification of what the format is supposed to be exactly. According to
               date fields in responses, it should probably be of the format
               '2014-08-31 07:41:44 GMT'
FORMAT     ::= :html | :markdown
               Specifies the markup in the text body, quote, caption, or whatever the current
               text-block container is.
SLUG       --- string
               A short string to use at the end of the URL to the post." type required optionals))

(docf blog/post-text
      (post-docstring
       "text"
       "~
BODY       --- string
               The main post body, marked up in FORMAT."
       "~
TITLE      --- string
               The title to use for your text post."))

(docf blog/post-quote
      (post-docstring
       "quote"
       "~
QUOTE      --- string
               The quote to use, marked up in FORMAT."
       "~
SOURCE     --- string
               Quote source attribution."))

(docf blog/post-link
      (post-docstring
       "link"
       "~
URL        --- string
               The link to post."
       "~
TITLE      --- string
               The title to use for your url post. If not specified automatically
               grabbed from the URL source page.
DESCRIPTION--- string
               Optional description text to use along your URL. If not specified
               automatically grabbed from the URL source page."))

(docf blog/post-chat
      (post-docstring
       "chat"
       "~
CONVERSATION-- string
               The conversation to post."
       "~
TITLE      --- string
               The title to use for your conversation post."))

(docf blog/post-photo
      (post-docstring
       "photo"
       "~
PHOTO      ::= string | pathname | octet-array | (pathname* octet-array*)
               If a string, it should be an URL to the photo to be posted.
               Otherwise a pathname/octet-array or a list thereof to compose
               the photo or photo-set."
       "~
CAPTION    --- string
               The caption to describe the photo/set with.
LINK       --- string
               An optional link to use for when the image is clicked on."))

(docf blog/post-audio
      (post-docstring
       "audio"
       "~
AUDIO      ::= string | pathname | octet-array
               If a string, it should be an external URL that hosts the file.
               Otherwise a pathname/octet-array of an MP3 file."
       "~
CAPTION    --- string
               The caption to describe the audio with."))

(docf blog/post-video
      (post-docstring
       "video"
       "~
VIDEO      ::= string | pathname | octet-array
               If a string, it should be the HTML embed-code for the video.
               Otherwise a pathname/octet-array of an MP4 video with AAC audio encoding."
       "~
CAPTION    --- string
               THe caption to describe the video with."))

(docf blog/post/edit
      "Edit the post with the given ID.
See the individual BLOG/POST-* functions for the argument explanations.

Returns the post ID.")

(docf blog/post/reblog
      "Reblog the post with the given ID on the specified BLOG.

ID          --- The post's unique ID.
REBLOG-KEY  --- The reblog-key for this post (See BLOG/POSTS)
COMMENT     --- An optional comment to add to the reblog.

Returns the reblog's post ID.")

(docf blog/post/delete
      "Delete the post with the given ID.

Returns the post's ID.")

;;;
;;;  POSTS.LISP
;;;

(docf blog/posts
      "Returns a list of posts from the blog.
Secondary value contains the total number of posts on the blog.
Tertiary value contains the blog object. See BLOG/INFO.

TYPE          --- The type of posts to include, must be one of
                  :text :quote :link :answer :video :audio :photo :chat
ID            --- If specified with a post-id, only that particular post is
                  returned.
TAG           --- Filter posts by a tag.
LIMIT         --- The number of posts to include, must range between 1 and 20.
OFFSET        --- The offset in posts to return.
REBLOG-INFO   --- Whether to return the various 'reblogged-' fields.
NOTES-INFO    --- Whether to return note count and metadata.
FILTER        --- The post format to return, either NIL for HTML, :text for
                  HTML-stripped text, or :raw for the text the user entered.

A post object contains the following fields:
BLOG-NAME     --- The hostname of the blog.
ID            --- The unique post-ID.
POST-URL      --- Direct URL to the post.
TYPE          --- The type of the post as a string.
TIMESTAMP     --- Unix-Timestamp of the post's creation date.
DATE          --- The 'GMT-formatted' version of the creation date.
FORMAT        --- Either 'html' or 'markdown'.
REBLOG-KEY    --- The key necessary to reblog this post.
TAGS          --- The list of tags as strings.
BOOKMARKLET   --- Only exists if true, signals that the post was made
                  using the tumblr bookmarklet.
MOBILE        --- Onlx exists if true, signals that the post was made
                  using mobile or email publishing.
SOURCE-URL    --- The source address of quotes, reblogs, etc. Exists only
                  if there is a source.
SOURCE-TITLE  --- The title of the source site, exists only if there is a
                  source.
LIKED         --- T if you have liked this post, NIL otherwise.
                  Only exists if you have been logged in.
STATE         --- The state of the post, one of 'published' 'queued' 'draft'
                  or 'private'.

Text-post attributes:
TITLE         --- The title of the post
BODY          --- The text-post's content.

Photo-post attributes:
PHOTOS        --- A list of photo objects with the following properties:
 CAPTION      --- Caption for this particular photo.
 ALT-SIZES    --- List of size objects with the following properties:
  WIDTH       --- The exact width of this size in pixels.
  HEIGHT      --- The exact height of this size in pixels.
  URL         --- The URL pointing to this image size.
CAPTION       --- The caption for this photo/set.
WIDTH         --- Width of the photo/set.
HEIGHT        --- Height of the photo/set.

Quote-post attributes:
TEXT          --- The quote text.
SOURCE        --- HTML of the supplied, supposed quote source.

Link-post attributes:
TITLE         --- The title of the page the link points to.
URL           --- The actual address of the link.
DESCRIPTION   --- An optional description supplied by the user.

Chat-post attributes:
TITLE         --- Optional title of the conversation.
BODY          --- The full conversation.
DIALOGUE      --- List of objects with the following properties:
 LABEL        --- Conversation entry label.
 NAME         --- Name of the speaker.
 PHRASE       --- Actual dialogue sentence.

Audio-post attributes:
CAPTION       --- The caption the user supplied for the audio post.
PLAYER        --- HTML player embed code.
PLAYS         --- Counter for the number of times this has been played.
ALBUM-ART     --- URL to the album-art image of the track. [ID3]
ARTIST        --- The track's artist. [ID3]
ALBUM         --- The track's album. [ID3]
TRACK-NAME    --- The name of the track. [ID3]
TRACK-NUMBER  --- The ordering number of the track. [ID3]
YEAR          --- The publishing year of the track. [ID3]

Video-post attributes:
CAPTION       --- The caption for the video post.
PLAYER        --- A list of players with the following properties:
 WIDTH        --- The width of the player object.
 EMBED-CODE   --- The HTML embed code for the video player.

Answer-post attributes:
ASKING-NAME   --- The name of the blog that asked the question.
ASKING-URL    --- Full URL to the asker's blog.
QUESTION      --- The actual question text.
ANSWER        --- Full text of the answer.")

(docf blog/posts/queue
      "Returns the list of posts in the blog's queue.
See BLOG/POSTS for the post object structure.

LIMIT         --- The number of posts to include, must range between 1 and 20.
OFFSET        --- The offset in posts to return.
FILTER        --- The post format to return, either NIL for HTML, :text for
                  HTML-stripped text, or :raw for the text the user entered.")

(docf blog/posts/draft
      "Returns the list of drafts on the blog.
See BLOG/POSTS for the post object structure.

BEFORE-ID     --- Returns posts that appear after this ID.
FILTER        --- The post format to return, either NIL for HTML, :text for
                  HTML-stripped text, or :raw for the text the user entered.")

(docf blog/posts/submission
      "Returns the list of submitted posts and asks.
See BLOG/POSTS for the post object structure.

OFFSET        --- The offset in posts to return.
FILTER        --- The post format to return, either NIL for HTML, :text for
                  HTML-stripped text, or :raw for the text the user entered.")

;;;
;;; TAGGED.LISP
;;;

(docf tagged
      "Returns the list of posts under the given tag.
See BLOG/POSTS for the post object structure.

TAG     --- The tag to filter for.
BEFORE  --- A Unix-Timestamp from before which time you'd like to get
            posts returned. Defaults to the current time.
LIMIT   --- The number of posts to include, must range between 1 and 20.
FILTER  --- The post format to return, either NIL for HTML, :text for
            HTML-stripped text, or :raw for the text the user entered.")

;;;
;;; USER.LISP
;;;

(docf user/info
      "Returns information about yourself.

FOLLOWING        --- Counter for the number of blogs you are following.
DEFAULT-POST-FORMAT  The default format to use for posting, one of
                     'html' 'markdown' 'raw'
NAME             --- The short name of the user.
LIKES            --- Counter for the number of likes.
BLOGS            --- A list of blog objects with the following properties:
 NAME            --- Short name of the blog.
 URL             --- The full URL to the blog.
 TITLE           --- The title string used on the blog.
 PRIMARY         --- T if this is the user's primary blog, NIL otherwise.
 FOLLOWERS       --- Counter for the number of followers on this blog.
 TWEET           --- String denoting what to do with tweets (?)
                     One of 'Y' 'N' 'auto', but idk what the difference
                     between 'Y' and 'auto' is, so.
 FACEBOOK        --- Whether to send posts to facebook, either 'Y' or 'N'.
 TYPE            --- Either 'public' or 'private'.

!! OFFICIALLY UNDOCUMENTED PROPERTIES
The following BLOG properties are also included in a request:
 DRAFTS          --- Number of drafts on the blog.
 QUEUE           --- Number of posts queued up.
 MESSAGES        --- The number of messages (asks) received.
 ADMIN           --- Whether you are an administrator of the blog.
 FACEBOOK-OPENGRAPH-ENABLED No idea wtf this is.
 TWITTER-SEND    --- Probably whether to send twitter messages? No idea.
 TWITTER-ENABLED --- Whether twitter was linked or not, most likely.
 LIKES           --- The number of posts this user-blog has liked.
 SHARE-LIKES     --- T if likes are shared, NIL otherwise.
 CAN-SEND-FAN-MAIL - Whether this blog can send fan mail (?)
 FOLLOWED        --- Not sure, maybe whether the blog follows you?
 ASK-ANON        --- Whether anon-asks are allowed.
 ASK-PAGE-TITLE  --- The ask-page title.
 ASK             --- Whether asks are enabled.
 IS-NSFW         --- T if this is a NSFW-blog.
 DESCRIPTION     --- The blog's description.
 UPDATED         --- Unix-Timestamp of the last update.
 POSTS           --- Counter for the number of posts on this blog.")

(docf user/dashboard
      "Returns a list of posts on your dashboard.
See BLOG/POSTS for the post object structure.

LIMIT         --- The number of posts to include, must range between 1 and 20.
OFFSET        --- The offset in posts to return.
TYPE          --- The type of posts to include, must be one of
                  :text :quote :link :answer :video :audio :photo :chat
SINCE-ID      --- Return only posts appearing after this post-ID.
REBLOG-INFO   --- Whether to return the various 'reblogged-' fields.
NOTES-INFO    --- Whether to return note count and metadata.")

(docf user/likes
      "Returns a list of posts you liked.
See BLOG/POSTS for the post object structure.

LIMIT         --- The number of posts to include, must range between 1 and 20.
OFFSET        --- The offset in posts to return.")

(docf user/following
      "Returns a list of blogs you follow.

LIMIT         --- The number of posts to include, must range between 1 and 20.
OFFSET        --- The offset in posts to return.

A blog object contains the following properties:
DESCRIPTION  --- The blog's description text.
UPDATED      --- Unix-Timestamp of the last update.
TITLE        --- Title used on the blog's page.
NAME         --- The blog's short name.
URL          --- The full URL to the blog.")

(docf user/follow
      "Follow a blog.

BLOG --- The blog's short name.

Returns T on success.")

(docf user/unfollow
      "Unfollow a blog.

BLOG --- The blog's short name.

Returns T on success.")

(docf user/like
      "Like a post.

ID          --- The post's unique ID.
REBLOG-KEY  --- The reblog-key for this post (See BLOG/POSTS)

Returns T on success.")

(docf user/unlike
      "Unlike a post.

ID          --- The post's unique ID.
REBLOG-KEY  --- The reblog-key for this post (See BLOG/POSTS)

Returns T on success.")


;;;
;;; OBJECTS.LISP
;;;

(docc blog
      "Class representing a blog.

This is not equivalent to a user. Each user is necessarily also a blog,
but not every blog is a user. One user may have access to multiple blogs.
Pure blogs can't follow or like anything or do any of the real-user
related things, but since most of the user settings are not actually
accessible through the API, the difference in slots between blogs and
users is minimal.")

(docc user
      "Class representing a user.

Every user is also a blog. See the BLOG class.")

(docc post
      "Superclass for posts. Should not be used directly.

Every type of post has a lot of fields in common with every other, this
class defines these common fields. Note that some fields, such as
TWEET, SLUG and POST-FORMAT are only used for creating or editing posts
and are never actually returned by the API.")

(docc text-post
      "Class representing a text post.")

(docc photo-post
      "Class representing a photo/set post.

In order to change or upload photos, please use the FILE slot.
Acceptable are a single string (representing the URL to a photo),
a pathname, an octet vector or a list of pathnames or octet vectors.")

(docc quote-post
      "Class representing a quote post.")

(docc link-post
      "Class representing a link post.")

(docc chat-post
      "Class representing a chat post.")

(docc audio-post
      "Class representing an audio post.

In order to change or upload audio files, please use the FILE slot.
Acceptable are a string (representing the URL to a music track),
a pathname or an octet vector. The file format has to be MP3.")

(docc video-post
      "Class representing a video post.

In order to change or upload video files, please use the FILE slot.
Acceptable are a string (being the HTML embedding code for the video),
a pathname or an octet vector. The file format has to be MP4 with
audio encoded in AAC.")

(docc answer-post
      "Class representing an answer post.")

(docc photo
      "Class representing a photo in a photo/set post.")

(docc photo-size
      "Class representing a photo-size of a photo.")

(docc dialogue
      "Class representing a dialogue entry in a chat-post.")

(docc video-player
      "Class representing a video player object in a video-post.")

;;;
;;; GENERICS.LISP
;;;

(docf blog=
      "Returns T if the two blogs match in name, NIL otherwise.")

(docf post=
      "Returns T if the two posts match in ID, NIL otherwise.")

(docf augment
      "Fills up slots in TARGET with information from SOURCE.")

(docf copy
      "Attempts to copy the passed object by creating a new instance of the same class and then calling AUGMENT on it.
Note that since AUGMENT does not copy fields, this is NOT a deep copy and thus slot values are shared!")

(docf blog
      "Tries to fetch the blog object given by NAME.

If a blog object is passed, the same post is fetched anew
and the new copy is returned. The old is left intact.

Returns a blog object or NIL if no blog could be found.")

(docf followers
      "Returns a list of USER objects that follow the given blog.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf follow
      "Follow the given blog.

Returns the blog on success, signals an error on failure.")

(docf unfollow
      "Unfollow the given blog.

Returns the blog on success, signals an error on failure.")

(docf likes
      "Retrieve a list of liked posts if possible.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf submissions
      "Retrieve a list of submitted posts if possible.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf drafts
      "Retrieve a list of drafted posts if possible.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf queue
      "Retrieve a list of queued posts if possible.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf posts
      "Retrieve a list of public posts filtered by TYPE and TAG.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf post
      "Retrieve an individual post by its unique ID.

If a blog object is passed, the same post is fetched anew
and the new copy is returned. The old is left intact.

Returns a post object or NIL if no post can be found.")

(docf save
      "Creates or edits the given post with the new values.

Returns the post object on success, signals an error on failure.")

(docf repost
      "Creates a copy of the post object and posts it to the given blog as a new post.

Returns the repost object on success, signals an error on failure.

Note that the slot values of the repost are shared with that of the original post.
See COPY.")

(docf destroy
      "Delete the given post.

Returns the post object on success, signals an error on failure.")

(docf reblog
      "Reblog the given post.

Returns the newly created reblog post object on success, signals
an error on failure.")

(docf like
      "Like the given post.

Returns the post object on success, signals an error on failure.")

(docf unlike
      "Unlike the given post.

Returns the post object on success, signals an error on failure.")

(docf refresh
      "Update the post object with newly retrieved values.

This changes the passed object's slots. See AUGMENT.

Returns the post object on success, signals an error on failure.")

(docf dashboard
      "Retrieve a list of posts on your dashboard.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf following
      "Retrieve a list of blogs that you are following.

If AMOUNT is above twenty, multiple requests are made in order to
gather the desired amount automatically.")

(docf myself
      "Retrieve information about yourself.

Returns a USER object.")

(docf my-followers
      "Shorthand for (FOLLOWERS *user*)")

(docf my-likes
      "Shorthand for (LIKES *user*)")

(docf my-submissions
      "Shorthand for (SUBMISSIONS *user*)")

(docf my-drafts
      "Shorthand for (DRAFTS *user*)")

(docf my-queue
      "Shorthand for (QUEUE *user*)")

(docf my-posts
      "Shorthand for (POSTS *user*)")
