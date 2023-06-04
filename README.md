# ai-blog.el

ai-blog.el is an Emacs package that leverages the power of OpenAI's ChatGPT, OpenAI's DALL-E, [Pexels](https://www.pexels.com/), Google Images (through [SerpApi](https://serpapi.com/)) and Bing Image Search to streamline the generation of blog posts and images in Hugo. The package provides functions to create content, generate tags, and insert images in your blog posts.

## Features

- Persona-based content generation
- Tag generation
- Downloading and inserting images into your blog posts, either as part of the content or as featured image

## Dependencies

- [emacs-easy-hugo](https://github.com/masasam/emacs-easy-hugo)
- [gptel](https://github.com/karthink/gptel)

You also need API keys for the services you want to use.

## Installation

Clone this repository into `~/.config/doom/functions/ai-blog/`.

Load it. For doom-emacs:

```emacs-lisp
;; file ~/.config/doom/packages.el

(package! ai-blog :recipe (:local-repo "~/.config/doom/functions/ai-blog/"))
```

```emacs-lisp
;; file ~/.config/doom/config.el

(use-package! ai-blog
  :demand t
  :after (gptel))
```

## Configuration

First, make sure you have API keys for the services you want to use. You can set them by adding the following lines to your configuration:

``` emacs-lisp
(setq ai-blog-dall-e-api-key "your_api_key_here")
(setq ai-blog-pexels-api-key "your_api_key_here")
(setq ai-blog-google-api-key "your_api_key_here")
(setq ai-blog-bing-api-key "your_api_key_here")
```

To get the API keys, follow each platform's instructions:

- [DALL-E](https://platform.openai.com/account/api-keys)
- [Pexels](https://help.pexels.com/hc/en-us/articles/900004904026-How-do-I-get-an-API-key-)
- [Google (through SerpApi)](https://serpapi.com/users/welcome)
- [Bing](https://www.microsoft.com/en-us/bing/apis/bing-image-search-api)

### Changing featured image field name

By default, ai-blog.el uses `image` as the field name for the post featured image, meaning it will add `image: "path/to/image.png"` to the front matter. To change that, use:

```emacs-lisp
(setq ai-blog-featured-image-field-name "FIELD_NAME")
```

### Image count

By default, ai-blog.el generated 5 images with DALL-E and retrieves 10 images with Pexels.

To change that:

```emacs-lisp
(setq ai-blog-dall-e-image-count 10)
(setq ai-blog-pexels-image-count 20)
(setq ai-blog-google-image-count 20)
(setq ai-blog-bing-image-count 20)
```

## Usage

### Blog post generation

To generate blog posts with ChatGPT:

``` emacs-lisp
(ai-blog-write-post "title")
```

#### Persona

To write blog posts, ai-blog.el uses a custom system prompt and a user prompt prefix.

To add a new persona, simply create files named `personas/NAME.system` and `personas/NAME.prompt`, and run:

```emacs-lisp
(ai-blog-select-persona)
```

### Tag generation

To generate SEO-friendly tags for your blog post:

``` emacs-lisp
(ai-blog-generate-tags)
```

### Insert images into blog posts

To insert an image into your blog post:

``` emacs-lisp
(ai-blog-insert-image-dall-e)
(ai-blog-insert-image-pexels)
(ai-blog-insert-image-google)
(ai-blog-insert-image-bing)
```

To insert a featured image in your blog post:

``` emacs-lisp
(ai-blog-insert-featured-image-dall-e)
(ai-blog-insert-featured-image-pexels)
(ai-blog-insert-featured-image-google)
(ai-blog-insert-featured-image-bing)
```

## Contributing

If you'd like to contribute to the project, please feel free to open issues or submit pull requests on the GitHub repository.

## Acknowledgements

- [sstraust](https://github.com/sstraust) for the Emacs image retrieving and selection logic
