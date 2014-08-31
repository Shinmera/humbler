#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defmacro docf (function docstring &rest format-args)
  `(setf (documentation ',function 'function)
         (format NIL ,docstring ,@format-args)))

(defun post-docstring (type required optionals)
  (format NIL "Creates a ~a post and returns the post's ID.

Required arguments:
USERNAME   --- string
               The username of the blog to post to. Your account has to have access
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
AUDIO      --- string | pathname | octet-array
               If a string, it should be an external URL that hosts the file.
               Otherwise a pathname/octet-array of an MP3 file."
       "~
CAPTION    --- string
               The caption to describe the audio with."))

(docf blog/post-video
      (post-docstring
       "video"
       "~
VIDEO      --- string | pathname | octet-array
               If a string, it should be the HTML embed-code for the video.
               Otherwise a pathname/octet-array of an MP4 video with AAC audio encoding."
       "~
CAPTION    --- string
               THe caption to describe the video with."))
