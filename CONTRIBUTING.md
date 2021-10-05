# Guide for Contributors – Serokell Blog

Contributing to the Serokell blog? This page documents how you should format & submit your article and what to keep in mind to write the most awesomest-and-Serokell-blog-like article possible. 

We’ll cover:

* How to write an awesome post.
* How to format your writing. 
* How to submit it. 
* What happens after you submit it.


## How to write a good article

Here’s our proven process for quickly chugging out amazing articles. It is focused on reducing procrastination/resistance and writing nicely structured articles.  

Of course, this is just a suggestion – if you have your own way that “works”, you can write like that. We just need the articles. 😅 

**Step 1: Imagine your reader & focus your topic.**

You are writing to help a person. Imagine a person, and where they encounter your article. What do they want to accomplish? What do they already know about the topic? What do they need to know? 

You can write that info down for yourself or just keep it in mind while writing. 

_It’s like making a user story._  

**Step 2: Make a structure beforehand.**

It’s much easier to make a structure of headings, subheadings, and descriptions before you start writing. This will guide your writing process and help you orient while you are in the middle of text. 

_It’s like writing types before functions._

**Step 3: Write it out.** 

Fill out the article from start to finish with content that fits the structure. The focus here is to just make it work. Do not think too much about quality. 

_In other words: make it compile._ 

**Step 4. Edit ruthlessly.**

Now that you have finished the text, go back and make it look good. This is best done on another day after writing. 

Make sure everything makes sense for the reader, the sentences and definitions are clear, and everything looks proper according to the style guide.  

_In other words: make it beautiful._


## Style guide 

Here are some of the things that all Serokell articles need to follow. Do what you can, the editor will enforce the rest. 

**Oxford comma**

Put a comma before and at the end of a list. 

Correct: We develop software in Haskell, Elixir, and Rust.

Incorrect: We develop software in Haskell, Elixir and Rust. 

**En dash**

Use en dashes, surrounded by spaces. Don’t use em dashes with no spaces or hyphens.   

Correct: Two things – Template Haskell and type-level programming – give me the most problems.

Incorrect: Two things - Template Haskell and type-level programming - give me the most problems. 

**Lists**

We prefer lists where each item is either without punctuation or a sentence that ends with a full stop. You can use semicolon lists if you want. Just be consistent across the article. 

More about lists: https://www.grammarly.com/blog/bullet-points/  

**Headers**

Title of the article is in the [title case](https://titlecase.com/). 

All other headers start from Header 2 (##+). They are in the [sentence case](https://www.thoughtco.com/sentence-case-titles-1691944#:~:text=Sentence%20case%20is%20the%20conventional,the%20standard%20form%20for%20headlines.). 

**Writing style**

Here are some tips that will make you write like a god: 

* Keep the language classy – no cursing, engaging in fervent statements, etc.  
* Write conversationally – you are writing for other human beings.
* Avoid passive voice like plague.   
* Keep the sentences clear and simple. Whenever possible, avoid lengthy, run-on sentences.
* Don’t use complicated words unless necessary. 
* Define the terms you are using (or link to definitions) unless they are common knowledge. 
* Use some kind of automatic checker (Grammarly) to make sure there are no typos in words. 

For more info on good style, we suggest browsing The Elements of Style: [http://www.jlakes.org/ch/web/The-elements-of-style.pdf](http://www.jlakes.org/ch/web/The-elements-of-style.pdf)


## Images

If you need an image or diagram for your article, we have designers to create them. Contact the editor with your wishes. 
 
If you use any other image, make sure it is at least **one of these**:

* Owned by Serokell.
* Appropriately licensed and attributed (CC, public domain, etc.).
* You have the permission of the owner to use it 


## Submitting the article 

You can submit your article in two different ways: via GitHub or via Google Docs.


### GitHub (for developers)

We have a [GitHub repository](https://github.com/serokell/blog-posts) for submitting blog posts. Once you have a draft, you can open a pull request there with: 



* A folder with the name of your article. 
* Draft as .md. 
* Any other images you want to use in the draft.

The draft should contain the text of your article in Markdown. 

Each sentence of the article should be in a new line, the lines should be unwrapped (this is to reduce the noise in diffs). Here’s a good example: [https://github.com/serokell/blog-posts/blob/master/lorentz-base/article.md](https://github.com/serokell/blog-posts/blob/master/lorentz-base/article.md)

You can also use [HTML](https://www.w3schools.com/html/) and [LaTeX](https://latex-tutorial.com/tutorials/amsmath/) in the post. 

**Regarding HTML:** Do not use HTML and Markdown in the same block – it will not render nicely. 

**Regarding LaTeX:** LaTeX expressions are surrounded by dollar signs. They can be either inline (one dollar sign) or block (two dollar signs).

Inline: 

```
$\operatorname{Cl}(\emptyset) = \emptyset$, empty set is closed.
```

Block:

```
$$Y = a + bX$$
```

### Google Docs (for non-developers)

The other option is to submit via Google Docs. This is a reasonable option for non-developers or for texts that don’t contain a lot of code. 

After editing, your article will be auto-converted to Markdown, which will erase colors and some formatting. **These are the things you can use to format your text:**



* Paragraph styles, such as Normal text, Heading 2, etc. (Accessible on the toolbar or in the Format menu.) 
* Monospace fonts to signify code blocks, such as Courier New. (Copying code straight from VSCode works.)
* Bold & italic. 
* Ordered & unordered lists.
* Horizontal lines.
* Tables.

**Don’t use the font size + bold formatting to make your own custom headings** or other elements, don’t colour text, don’t underline. 

Attach all the images you use as an archive to the corresponding issue – Google Docs doesn’t play well with images, for some reason. 


## Reviews & Proof-reading 

After submission, your article will (hopefully) be reviewed by 1 or 2 people that are knowledgeable in the area. You can suggest reviewers of your own. At this step, the editor might also suggest structural improvements.  
 
After the review process is over, the editor will make sure it is grammatically correct and presentable.

When the time comes, the article will be published by the editor. You will be informed when this happens. 
