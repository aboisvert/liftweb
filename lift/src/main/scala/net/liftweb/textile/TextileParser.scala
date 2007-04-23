package net.liftweb.textile

/*                                                *\
 (c) 2006-7 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.util.parsing.Parsers
import scala.xml.{MetaData, NodeSeq, Elem, Null, Text, TopScope, Unparsed, UnprefixedAttribute}
import scala.collection.mutable.HashMap


/**
 * A parser that deals with 1 character at a time
 * Based on the Scala parser example code
 */
trait CharParsers extends Parsers {
  def any: Parser[char]
  def chr(ch: char) = {
    for (val c <- any; c == ch) yield c
  }
  def chr(p: char => boolean) =
    for (val c <- any; p(c)) yield c
  
  /**
   * Match a list of characters
   */
  def str(in : List[char]) : Parser[char] = {
    (in: @unchecked) match {
      case c :: Nil => chr(c)
      case c :: rest => chr(c) &&& str(rest)
    }
  }
  
  /**
   * Match a string
   */
  def str(in : String) : Parser[char] = {
    if (in.length == 1) chr(in.charAt(0))
    else chr(in.charAt(0)) &&& str(in.substring(1))
  }

}

/**
* The textile parser
*/ 
object TextileParser {

  /**
  * Take a string and return the parsed value.
  * Lst is a list of Textile parsed elements.  You can do a .toHtml on the Lst
  * to get the XHTML result to send to the browser.
  * int will be the number of characters parsed.
  */
  def parse(toParse : String) : Option[Pair[Lst, int]] = {
    val prep = prepare(toParse)
    val parser = new ParseString(prep) with TextileParsers
    parser.parseAsTextile
  }
  
  def toHtml(toParse: String): NodeSeq = {
    parse(toParse).map(_._1.toHtml) getOrElse Text("")
  }

  /**
  * a class that will parse a string.  Based on
  * the scala parser example code.
  */
  class ParseString(s : String) extends Parsers {
    type inputType = int
    
    def input = 0
    def any = new Parser[char] {
      def apply(in: int): Parser[char]#Result =
        if (in < s.length()) Some(Pair(s charAt in, in + 1)) else None
    }
  }
  
  /**
  * the thing that actually does the textile parsing
  */
  trait TextileParsers extends CharParsers {
    def input : inputType
    
    /**
    * is it a blank line?  Start of line, some spaces, and an end of line
    */
    def blankLine: Parser[Textile] = {
      for (
        val c: char <- chr('\0');
        val spaces <- rep(chr(' '));
        val cs: char <- chr('\n')
      ) yield BlankLine
    }

    /**
    * Line elements make up paragraphs and block elements
    */
    def lineElem : Parser[Textile] = {
      not(blankLine) &&& (beginingOfLine ||| endOfLine ||| image ||| footnote_def ||| 
			  anchor ||| dimension ||| elipsis  ||| 
			  copyright ||| trademark ||| registered ||| emDash ||| 
			  enDash ||| italic ||| emph ||| bold  ||| 
			  cite |||  span ||| code ||| delete ||| insert ||| 
			  sup ||| sub ||| strong ||| html ||| single_quote ||| quote ||| acronym ||| charBlock)
    }

    /**
    * If we've got an italic (__text__), the parser doesn't do well with a single underscore, so
    * we exclude looking for _emph_
    */
    def lineElem_notEmph : Parser[Textile] = {
      not(blankLine) &&& (beginingOfLine ||| endOfLine ||| image ||| footnote_def ||| anchor ||| 
			  dimension ||| elipsis ||| 
			  copyright ||| trademark ||| registered ||| emDash ||| enDash ||| italic ||| 
			  bold  ||| 
			  cite |||  span||| code ||| delete ||| insert||| sup ||| sub ||| strong  ||| 
			  html||| single_quote ||| quote ||| acronym ||| charBlock)        
    }

    /**
    * Don't look for *strong* if we're currently in a **bold** element
    */
    def lineElem_notStrong : Parser[Textile] = {
      not(blankLine) &&& (beginingOfLine ||| endOfLine ||| image ||| footnote_def ||| anchor ||| 
			  dimension ||| elipsis ||| 
			  copyright ||| trademark ||| registered ||| emDash ||| enDash ||| italic ||| 
			  emph ||| 
			  cite |||  span ||| code ||| delete ||| insert  ||| sup ||| 
			  sub ||| bold  ||| html||| single_quote ||| quote ||| acronym  ||| charBlock)
    }
    

    /**
    * Look for an acronym, but don't mistake registered, copyright, and trademarks
    */
    def acronym : Parser[Textile] = {
      for (
        val thing : List[char] <- rep1(chr(&acro_thing));
        val op <- not(copyright ||| trademark ||| registered) &&& str("(");
        val acro <- rep1(chr(&acro));
        val cp <- str(")")
      ) yield Acronym(thing.mkString("","",""), acro.mkString("","",""))
    }
    
    
    private def acro(c : char) : boolean = c != ')' && c != '\n';
    
    private def acro_thing(c : char) : boolean = c != '\n' && c != ' ' && c != '('
    
    /**
    * is it an !image!
    */
    def image : Parser[Textile] = {
      for (
        val c1 <- chr('!');
        val fl <- opt(chr('<'));
        val fr <- opt(chr('>'));
        val img_url : List[char] <- rep1(not(chr('!')) &&& chr(validUrlChar));
        val alt : List[List[char]] <- opt(img_alt);
        val ce <- chr('!');
        val link <- opt(img_link)
      ) yield Image(img_url.mkString("","",""), if (alt.length > 0) alt.head.mkString("","","") else "",
		    if (link.length > 0) link.head.href else null, 
		    if (fl.length > 0) List(AnyAttribute("style", "float:left"))
		    else if (fr.length > 0) List(AnyAttribute("style", "float:right"))
		    else Nil)
    }
    
    private def img_alt  = {
      for (
        val c1 <- str("(");
        val ret <- rep1(not(str(")")) &&& chr(&not_eol));
        val c2 <- str(")")
      ) yield ret
    }
    
    private def img_link : Parser[Anchor] = 
      for (
        val c <- chr(':');
        val anchor : Textile <- url;
        anchor.isInstanceOf[Anchor]
      ) yield anchor.asInstanceOf[Anchor]

    
    /**
    * [footnote]
    */
    def footnote_def : Parser[Textile] = {
      for (
        val c1 <- chr('[');
        val nr : List[char] <- rep1(chr(isDigit));
        val c2 <- chr(']')
      ) yield FootnoteDef((nr).mkString("","",""))
    }
    
    def isUpper : (char) => boolean = Character.isUpperCase
    def isLower : (char) => boolean = Character.isLowerCase
    def isLowerOrNumber : (char) => boolean = _isLowerOrNumber
    def _isLowerOrNumber(c : char) = Character.isLowerCase(c) || Character.isDigit(c)
    
    
    /**
    * various things that make up an anchor (a tag)
    */
    def anchor = url ||| quote_url ||| quote_ref ||| a_ref ||| escCamelUrl ||| camelUrl
    
    /**
    * Don't use the CamelCase thing for a wikiword if it's prefixed by
    * a backslash
    */
    def escCamelUrl : Parser[Textile] = {
      for (
        val esc : char <- chr('\\');
        val fc1 : char <- chr(isUpper);
        val fc2 : char <- chr(isLower);
        val fcr : List[char] <- rep(chr(isLowerOrNumber));
        
        val sc1 : char <- chr(isUpper);
        val sc2 : char <- chr(isLower);
        val scr : List[char] <- rep(chr(isLowerOrNumber));
        
        val sc3 : List[List[char]] <- rep(chr(isUpper) &&& chr(isLower) &&& rep(chr(isLowerOrNumber)))
      ) yield CharBlock(((esc :: fc1 :: fc2 :: fcr) ::: (sc1 :: sc2 :: scr) ::: 
			 (sc3.flatMap{a => a})).mkString("","",""))
    }
    
    /**
    * is the work camelized?
    */
    def camelizedWord : Parser[CharBlock] = {
      for (
        val fc1 : char <- chr(isUpper);
        val fc2 <- chr(isLower);
        val fcr <- rep(chr(isLowerOrNumber))
      ) yield CharBlock((fc1 :: fc2 :: fcr).mkString("","",""))
      
    }
    
    /**
    * a WikiWord
    */
    def camelUrl : Parser[Textile] = {
      for (
        val fc1 <- camelizedWord;
        val sc1 <- camelizedWord;
        val sc3 : List[CharBlock] <- rep(camelizedWord)
      ) yield WikiAnchor(Nil, ((fc1 :: sc1 :: sc3).map{a => a.s}).mkString("","","") ,
			 ((fc1 :: sc1 :: sc3).map{a => a.s}).mkString("","",""), Nil)
      
    }
    
    /**
    * "reference":reference
    */
    def quote_ref : Parser[Textile] = {
      for (
        val qt : char <- chr('"');
        val fs : List[char] <- rep1(not(chr('"')) &&& chr(&not_eol));
        val eq <- str("\":");          
        val rc : List[char] <- rep1(chr(validUrlChar))
      ) yield RefAnchor(Nil, (rc).mkString("","",""),
			(fs).mkString("","",""), Nil)      
    }
    
    /**
    * "google":http://google.com
    */
    def quote_url : Parser[Textile] = {
      for (
        val qt : char <- chr('"');
        val fs : List[char] <- rep1(not(chr('"')) &&& chr(&not_eol));
        val eq <- str("\":");          
        val http : char <- str("http");
        val s : List[char] <- opt(chr('s'));
        val css : char <- str("://");
        val rc : List[char] <- rep1(chr(validUrlChar))
      ) yield Anchor(Nil, "http" + s.mkString("","","") + "://"+ (rc).mkString("","",""),
		     (fs).mkString("","",""), Nil)      
    }
    
    /**
    * [reference]:http://reference.com
    */

    def a_ref : Parser[Textile] = {
      for (
        val c1 : char <- chr('[');
        val fr : List[char] <- rep1(chr(validUrlChar));
        val c2 : char <- chr(']');
        val url <- url
      ) yield ARef((fr).mkString("","",""), url.asInstanceOf[Anchor].href)
    }

    /**
    * http://google.com
    */
    def url : Parser[Textile] = {
      for (
        val http : char <- str("http");
        val s : List[char] <- opt(chr('s'));
        val css : char <- str("://");
        val rc : List[char] <- rep1(chr(validUrlChar))
      ) yield Anchor(Nil, "http" + s.mkString("","","") + "://"+ (rc).mkString("","",""),
		     "http" + s.mkString("","","") + "://"+ (rc).mkString("","",""), Nil)
    }
    
    /**
    * a valid character in a URL
    */
    def _validUrlChar(c : char) : boolean = {
      Character.isLetterOrDigit(c) || c == '/' || c == '%' || c == '&' || c == '?' || c == '#' || 
      c == '$' || c == '.' ||
      c == '-' || c == ':'
    }
    
    def validUrlChar : (char) => boolean = _validUrlChar
    
    /**
    * an EOL character
    */
    def endOfLine : Parser[Textile] = 
      for (
        val s1 <- chr('\n')
      ) yield EOL
    
    /**
    * if we're in a &lt;pre&gt; block, an end of line is just an end of line.  We 
    * pass the '\n' on though.
    */
    def preEndOfLine : Parser[Textile] = 
      for (
        val s1 <- chr('\n')
      ) yield CharBlock("\n")
    
    
    /**
    * a &lt;pre&gt; block.  Just send text of through, unmolested.
    */
    def preBlock : Parser[Textile] = {
      for (
        val c1  <- chr('\0');
        val c2 <- rep(chr(' '));
        val s1 <- str("<pre");
        val c3 <- rep(chr(' '));
	val c4 <- chr('>');
        val elms <- rep(not(str("</pre")) &&& (preEndOfLine ||| beginingOfLine ||| charBlock ));
        val c5 <- str("</pre");
        val c6 <- rep(chr(' '));
        val c7 <- chr('>');
        val c8 <- rep(chr(' '));
        val c9 <- chr('\n')
      ) yield Pre(reduceCharBlocks(elms), Nil)
    }      
    
    /**
    * Begining of a line
    */
    def beginingOfLine : Parser[Textile] = 
      for (
        val s1 <- chr('\0')
      ) yield BOL
    
    /**
    * (c)
    */
    def copyright : Parser[Textile] = 
      for (
        val s1 <- chr('(');
        val s2 <- (chr('c') ||| chr('C'));
        val s3 <- chr(')')
      ) yield Copyright
    
    
    def _validTagChar(c : char) = {
      Character.isDigit(c) || Character.isLetter(c) || c == '_'
    }
    
    def validTagChar : (char) => boolean = _validTagChar
    
    def _validStyleChar(c : char) = {
      Character.isDigit(c) || Character.isLetter(c) || c == '.' || c == ' ' || c == ';' || c == '#'
    }

    def validStyleChar : (char) => boolean = _validStyleChar

    def _validClassChar(c : char) = {
      Character.isDigit(c) || Character.isLetter(c) || c == '.'
    }

    def validClassChar : (char) => boolean = _validClassChar

    def validTag : List[Parser[char]] = {
      chr(validTagChar) :: validTag
    }
    
    /**
    * An HTML block is made up of single HTML tag (e.g., &lt;br /&gt;) and 
    * tags that contain other stuff
    */
    def html = single_html ||| multi_html
    
    /**
    * an HTML tag that contains other stuff
    */
    def multi_html : Parser[Textile] = 
      for (
        val c1 <- str("<") ;
        val tag : List[char] <- rep1(chr(validTagChar));

        val attrs : List[Attribute] <- rep(tag_attr);
        val c2 : char <- chr('>');
        val body <- rep(not((str("</") &&& str(tag) &&& rep(chr(' ')) &&& chr('>'))) &&& (lineElem ||| paragraph));
        val e1 <- str("</");
        val et <- str(tag);
        val sp2 <- rep(chr(' '));
        val e2 <- chr('>');
        isValidTag((tag).mkString("","","")) // && showBody("body ", body) && showBody("body2 ",body2)
      ) yield HTML((tag).mkString("","",""), reduceCharBlocks(body), attrs)
    
    /**
    * A stand-alone tag
    */
    def single_html : Parser[Textile] = 
      for (
        val c1 <- str("<") ;
        val tag : List[char] <- rep1(chr(validTagChar));
        val attrs : List[Attribute] <- rep(tag_attr);
        val c2 : char <- str("/>");
        isValidTag((tag).mkString("","",""))
      ) yield HTML((tag).mkString("","",""), Nil, attrs)

    def tag_attr = single_quote_attr ||| double_quote_attr
    
    def attr_name(c : char) = Character.isLetterOrDigit(c) || c == '_' || c == '-'

    def attr_value(c : char) = {c >= ' '}


    /**
    * an attribute with single quotes
    */
    def single_quote_attr : Parser[Attribute] = {
      for (
        val sp <- rep(chr(' '));
        val name : List[char] <- rep1(chr(&attr_name));
        val eq <- str("='");
        val value : List[char] <- rep(not(chr('\'')) &&& chr(&attr_value));
        val end <- chr('\'')
      ) yield AnyAttribute((name).mkString("","",""), value.mkString("","",""))
    }
    
    /**
    * an attribute with double quotes
    */
    def double_quote_attr : Parser[Attribute] = {
      for (
        val sp <- rep(chr(' '));
        val name : List[char] <- rep1(chr(&attr_name));
        val eq <- str("=\"");
        val value : List[char] <- rep(not(chr('"')) &&& chr(&attr_value));
        val end <- chr('"')
      ) yield AnyAttribute((name).mkString("","",""), value.mkString("","",""))
    }
    
    /**
    * is it a valid HTML tag?  This list should be expanded
    */
    def isValidTag(in : String) = in == "b" || in == "em" || in == "code" || in == "div" || in == "span"


    /**
    * A "dimension" pretty printer
    */
    def dimension : Parser[Textile] = 
      for (
        val s1 <- str(" x ")
      ) yield Dimension

    def registered : Parser[Textile] = 
      for (
        val s1 <- chr('(');
        val s2 <- (chr('r') ||| chr('R'));
        val s3 <- chr(')')
      ) yield Register

    def trademark : Parser[Textile] = 
      for (
        val s1 <- str("(");
        val s2 <- (chr('t') ||| chr('T'));
        val s2a <- (chr('m') ||| chr('M'));
        val s3 <- str(")")
      ) yield Trademark
    
    def elipsis : Parser[Textile] = 
      for (
        val s1 <- str("...")
      ) yield Elipsis

    def emDash : Parser[Textile] = 
      for (
        val s1 <- str(" -- ")
      ) yield EmDash

    def enDash : Parser[Textile] = 
      for (
        val s1 <- str(" - ")
      ) yield EnDash

    def single_quote : Parser[Textile] = 
      for (
        val s1 <- str("'")
      ) yield SingleQuote

    def bold : Parser[Textile] = 
      for (
        val s1 <- str("**");
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(str("**")) &&& lineElem_notStrong); 
        val s2 <- str("**")
      ) yield Bold(reduceCharBlocks(ln), attrs)
    
    def bullet(depth : int, numbered : boolean) : Parser[Textile] = {
      for (
        val fbl <- bullet_line(depth, numbered);
        val sbl <- bullet(depth + 1, numbered) ||| bullet_line(depth, numbered);
        val abl <- rep(bullet(depth + 1, numbered) ||| bullet_line(depth, numbered))
      ) yield Bullet(fbl :: sbl :: abl, numbered)
    }
    
    def begin_bullet(depth : int, numbered: boolean) : Parser[char] = {
      if (depth == 0) (rep(chr(' ')) &&& chr(if (numbered) '#' else '*'))
					     else (rep(chr(' ')) &&& chr(if (numbered) '#' else '*')) &&& begin_bullet(depth - 1, numbered)
    }

    def bullet_line(depth : int, numbered : boolean) : Parser[Textile] = {
      for (
        val bol <- chr('\0');
        val sp <- begin_bullet(depth, numbered);
        val elms : List[Textile] <- rep(not(chr('\n')) &&& lineElem);
        val eol <- chr('\n')
      ) yield BulletLine(reduceCharBlocks(elms), Nil)
    }
    
    
    def strong : Parser[Textile] = {
      for (
        val s1 <- chr('*');
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(chr('*')) &&& lineElem); 
        val s4 <- chr('*')
      ) yield Strong(reduceCharBlocks(ln), attrs)
    }
    
    def cite : Parser[Textile] = {
      for (
        val s1 <- str("??");
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(str("??")) &&& lineElem); 
        val s4 <- str("??")
      ) yield Cite(reduceCharBlocks(ln), attrs)
    }
    
    def code : Parser[Textile] = {
      for (
        val s1 <- chr('@');
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(chr('@')) &&& lineElem); 
        val s4 <- chr('@')
      ) yield Code(reduceCharBlocks(ln), attrs)
    }
    
    def styleElem : Parser[StyleElem] = {
      for (
        val ncl : List[char] <- rep1(chr(validStyleChar));
        val c <- chr(':');
        val vcl : List[char] <- rep1(chr(validStyleChar))
      )
      yield StyleElem((ncl).mkString("","",""), (vcl).mkString("","",""))
    }
    
    def attribute = style ||| class_id ||| the_class ||| the_id ||| the_lang 
    
    def para_attribute = attribute ||| fill_align ||| left_align ||| right_align |||
    center_align ||| top_align ||| bottom_align ||| em_left ||| em_right;
    
    def left_align : Parser[Attribute] = for (val ac : char <- chr('<')) yield Align(ac)
    
    def right_align : Parser[Attribute] = for (val ac : char <- chr('>')) yield Align(ac)
    
    def center_align : Parser[Attribute] = for (val ac : char <- chr('=')) yield Align(ac)
    
    def top_align : Parser[Attribute] = for (val ac : char <- chr('^')) yield Align(ac)
    
    def bottom_align : Parser[Attribute] = for (val ac : char <- chr('~')) yield Align(ac)
    
    def fill_align : Parser[Attribute] = for (val ac <- str("<>")) yield Align('f')
    
    def em_left : Parser[Attribute] = {
      for (val ac : char <- chr('(');
           val acl : List[char] <- rep(chr('('))
	 ) yield Em(1 + acl.length)
    }

    def em_right : Parser[Attribute] = {
      for (val ac : char <- chr(')');
           val acl : List[char] <- rep(chr(')'))
         ) yield Em((1 + acl.length) * -1)
    }
    
    def class_id : Parser[Attribute] = {
      for (
        val cc <- chr('(');
        val rc : List[char] <- rep1(chr(validClassChar));
        val ps <- chr('#');
        val ri : List[char] <- rep1(chr(validClassChar));
        val c2 <- chr(')')
      ) yield ClassAndId((rc).mkString("","",""), (ri).mkString("","",""))
    }

    def the_id : Parser[Attribute] = {
      for (
        val cc <- chr('(');
        val ps <- chr('#');
        val ri : List[char] <- rep1(chr(validClassChar));
        val cc2 <- chr(')')
      ) yield ClassAndId(null, (ri).mkString("","",""))
    }

    def the_lang : Parser[Attribute] = {
      for (
        val ps <- chr('[');
        val ri : List[char] <- rep1(chr(validClassChar));
        val ps2 <- chr(']')
      ) yield Lang( (ri).mkString("","",""))
    }

    def the_class : Parser[Attribute] = {
      for (
        val cc <- chr('(');
        val rc : List[char] <- rep1(chr(validClassChar));
        val cc2 <- chr(')')
      ) yield ClassAndId((rc).mkString("","",""),null)
    }
    
    
    
    def style : Parser[Attribute] = {
      for (
        val cb1 <- str("{");
        val se : StyleElem <- styleElem;
        val sel : List[StyleElem] <- rep(chr(';') &&& styleElem);
        val cb2 <- str("}")
      ) yield Style(se :: sel)
    }
    
    def span : Parser[Textile] = {
      for (
        val s1 <- chr('%');
        val style : List[Attribute] <- opt(style); 
        val ln <-  rep1(not(chr('%')) &&& lineElem); 
        val s4 <- chr('%')
      ) yield Span(reduceCharBlocks(ln), style)
    }
    
    def delete : Parser[Textile] = {
      for (
        val s1 <- str("-");
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(str("-")) &&& lineElem); 
        val s4 <- str("-")
      ) yield Delete(reduceCharBlocks(ln), attrs)
    }
    
    def insert : Parser[Textile] = {
      for (
        val s1 <- str("+");
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(str("+")) &&& lineElem); 
        val s4 <- str("+")
      ) yield Ins(reduceCharBlocks(ln), attrs)
    }
    
    def sup : Parser[Textile] = {
      for (
        val s1 <- chr('^');
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(chr('^')) &&& lineElem); 
        val s4 <- chr('^')
      ) yield Super(reduceCharBlocks(ln), attrs)
    }
    
    def sub : Parser[Textile] =
      for (
        val s1 <- chr('~');
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(chr('~')) &&& lineElem); 
        val s4 <- chr('~')
      ) yield Sub(reduceCharBlocks(ln), attrs)
    
    def italic : Parser[Textile] = 
      for (
        val s1 <- str("__");
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(str("__")) &&& lineElem_notStrong); 
        val s2 <- str("__")
      ) yield Italic(reduceCharBlocks(ln), attrs)
    
    def emph : Parser[Textile] = {
      for (
        val s1 <- chr('_');
        val attrs : List[Attribute] <- rep(attribute);
        val ln <-  rep1(not(chr('_')) &&& lineElem); 
        val s4 <- chr('_')
      ) yield Emph(reduceCharBlocks(ln), attrs)
    }
    
    def quote : Parser[Textile] =
      for (
        val s1 <- chr('"');
        val ln <-  rep1(not(chr('"')) &&& lineElem); 
        val s4 <- chr('"')
      ) yield Quoted(reduceCharBlocks(ln))
    
    def reduceCharBlocks(in : List[Textile]) : List[Textile] = 
      (in: @unchecked) match {
        case Nil => {Nil}
        case EOL() :: BOL() :: rest => {EOL :: reduceCharBlocks(rest)}
        case EOL() :: rest => {reduceCharBlocks(rest)}
        case CharBlock(s1) :: CharBlock(s2) :: rest => {reduceCharBlocks(CharBlock(s1 + s2) :: rest)}
        case x :: xs => {x :: reduceCharBlocks(xs)}
      }

    def not_eol(c : char) = c != '\n'
    
    def charBlock : Parser[Textile] = 
      for (
        val c : char <- chr(&not_eol)
      ) yield CharBlock(c.toString)

    
    
    def blankPara : Parser[Textile] =
      for (
        val b1 <- blankLine;
        val bls <- rep(blankLine)
      ) yield BlankLine
    
    def not_blank_line : Parser[Textile] = not(blankLine) &&& lineElem
    
    def head_line : Parser[Textile] =
      for (val sc : char <- chr('\0') ;
	   val h : char <- chr('h') ;
	   val n : char <- (chr('1') ||| chr('2') ||| chr('3')) ;
	   val attrs : List[Attribute] <- rep(para_attribute);
	   val dot <- str(". ") ;
           val ln <- rep1(not_blank_line);
           val bl <- blankLine
           
	 ) yield Header(n - '0', reduceCharBlocks(ln), attrs)
    
    def blockquote : Parser[Textile] =
      for (val sc   <- str("\0bq");
           val attrs : List[Attribute] <- rep(para_attribute);
           val sc2 <- str(". ");
           val ln <- rep1(not_blank_line);
           val bl <- blankLine
	 ) yield BlockQuote(reduceCharBlocks(ln), attrs)

    
    def footnote : Parser[Textile] = {
      for (val sc   <- str("\0fn");
           val dr <- rep1(chr(isDigit));
           val attrs : List[Attribute] <- rep(para_attribute);
           val sc2 <- str(". ");
           val ln <- rep1(not_blank_line);
           val bl <- blankLine
           
	 ) yield Footnote(reduceCharBlocks(ln), attrs, (dr).mkString("","",""))
    }
    
    def first_paraAttrs : Parser[TableDef] = {
      for (
        val fc <- chr('\0');
        val sp1 <- rep(chr(' '));
        val table <- str("p");
        val style <- rep(para_attribute);
        val dot <- chr('.')
      ) yield TableDef(style)
    }
    
    def normPara : Parser[Textile] =
      for (
        val td : List[TableDef] <- opt(first_paraAttrs);
        val fp <- rep1(not_blank_line);
        val bl <- blankLine
      ) yield Paragraph(reduceCharBlocks(fp), if (td == Nil) Nil else td.head.attrs)
    
    def table_attribute = para_attribute ||| row_span ||| col_span
    
    def isDigit : (char) => boolean = Character.isDigit
    
    def row_span : Parser[Attribute] = {
      for (
        val c <- chr('/');
        val dr : List[char] <- rep1(chr(isDigit))
      ) yield AnyAttribute("rowspan", (dr).mkString("","",""))
    }

    
    def col_span : Parser[Attribute] = {
      for (
        val c <- chr('\\');
        val dr : List[char] <- rep1(chr(isDigit))
      ) yield AnyAttribute("colspan", (dr).mkString("","",""))
    }

    def table : Parser[Textile] = {
      for (
        val td <- opt(table_def);
        val th <- opt(table_header);
        val tr1 <- table_row;
        val trr <- rep(table_row)
      ) yield Table(th ::: tr1 :: trr, if (td == Nil) Nil else td.head.attrs)
    }
    
    def table_def : Parser[TableDef] = {
      for (
        val fc <- chr('\0');
        val sp1 <- rep(chr(' '));
        val table <- str("table");
        val style <- rep(para_attribute);
        val dot <- chr('.');
        val sp <- rep(chr(' '));
        val ec <- chr('\n')
      ) yield TableDef(style)
    }
    
    def row_def : Parser[TableDef] =
      for (
        val sr <- rep1(table_attribute);
        val dot <- chr('.')
      ) yield TableDef(sr)

    def table_row : Parser[Textile] =
      for (
        val bol <- chr('\0');
        val sp <- rep(chr(' '));
        val td <- opt(row_def);
        val sp <- rep(chr(' '));
        val fb <- chr('|');
        val re <- rep1(table_element(false));
        val spe <- rep(chr(' '));
        val eol <- chr('\n')
      ) yield TableRow(re, if (td == Nil) Nil else td.head.attrs)

    def table_header : Parser[Textile] = 
      for (
        val bol <- chr('\0');
        val sp <- rep(chr(' '));
        val td <- opt(row_def);
        val sp <- rep(chr(' '));
        val fb <- str("|_.");
        
        val fe <- table_element(true);
        val re <- rep(str("_.") &&& table_element(true));
        val spe <- rep(chr(' '));
        val eol <- chr('\n')
      ) yield TableRow(fe :: re, if (td == Nil) Nil else td.head.attrs)
    
    def table_element(isHeader : boolean) : Parser[Textile] =
      for (
        val td <- opt(row_def);
        val el <- rep(not(chr('|') ||| chr('\n')) &&& lineElem);
        val pipe <- chr('|')
      ) yield TableElement(reduceCharBlocks(el), isHeader, if (td == Nil) Nil else td.head.attrs)


    def paragraph : Parser[Textile] = 
      preBlock ||| footnote ||| table ||| bullet(0, false) ||| bullet(0, true) ||| blockquote ||| head_line ||| blankPara ||| normPara
    
    def parseAsTextile : Option[Pair[Lst, inputType]] = {
      val ret : Parser[Lst] = for (
        val pl <- rep(paragraph)
      ) yield Lst(pl)
      
      def findRefs(in : List[Textile]) : List[Pair[String,String]] = {

        in match {
          case (s : ARef) :: rest => {Pair(s.name, s.href) :: findRefs(rest)}
          case (s : ATextile) :: rest => {findRefs(s.theElems) ::: findRefs(rest)}
          case x :: rest => {findRefs(rest)}
          case _ => Nil
        }
      }

      def fixRefs(in : List[Textile], refs : HashMap[String, String]) : unit = {
        in match {
          case (s : RefAnchor) :: rest => {refs.get(s.ref) match {case Some(tr) => {s.href = tr} case None => {s.href = "#"}}; fixRefs(rest, refs)}
          case (s : ATextile) :: rest => {fixRefs(s.theElems, refs); fixRefs(rest, refs)}
          case s :: rest => {fixRefs(rest, refs)}
          case _ => {}
        }
        
      }

      val realRet = ret(this.input)
      val it = ret(this.input).get._1
      val tr = findRefs(List(it))

      val refs = new HashMap[String,String];
      tr.foreach{b => refs(b._1) = b._2}

      fixRefs(List(it), refs)

      // System.exit(0)
      Some(Pair(it,realRet.get._2))
    }
  }
  
  def prepare(in : String) : String = {
    def loop(x : List[Char]) : List[Char] = x match {
      case '\n' :: rest => '\n' :: '\0' :: loop(rest)
      case '\r' :: rest => loop(rest)
      case '\0' :: rest => loop(rest)
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }

    List.toString('\0' :: loop(List.fromString(in +'\n'+'\n'))) +'\n'
  }
  

  
  
  def tryit = {
    val prep = prepare(example)

    val ps = new ParseString(prep) with TextileParsers
    ps.parseAsTextile match {
      case Some(p) => {p._1.performOnWikiAnchor{a => a.rootUrl = "/foo/"}; p._1.toHtml}
      case _ => ""
    }
  }

  abstract class Textile {
    def toHtml : NodeSeq
  }
  
  case class Style(elms : List[StyleElem]) extends Attribute {
    def name(which : int) : String = "style"
    def value(which : int) : String = elms.mkString("",";","")
  }

  abstract class Attribute extends Textile {
    def toHtml = null
    def name(which : int) : String
    def value(which : int) : String
    def fieldCnt = 1
    def toList : List[Pair[String, String]] = {
      var ret : List[Pair[String,String]] = Nil
      var cnt = fieldCnt
      while (cnt > 0) {
	cnt = cnt -1
	val tn = name(cnt)
	val tv = value(cnt)
	if ((tn ne null) && (tv ne null)) ret = Pair(tn,tv) :: ret
      }
      ret
    }
  }

  case class AnyAttribute(name : String, value : String) extends Attribute {
    def name(which : int) : String = name
    def value(which : int) : String = value
  }

  case class ClassAndId(className : String, idName : String) extends Attribute {
    override def fieldCnt = 2
    def name(which : int) : String = {
      which match {
	case 0 => if ((className ne null) && className.length > 0) "class" else null
	case 1 => if ((idName ne null) && idName.length > 0) "id" else null
      }
    }
    def value(which : int) : String = {
      which match {
	case 0 => if ((className ne null) && className.length > 0) className else null
	case 1 => if ((idName ne null) && idName.length > 0) idName else null
      }
    }
    
  }

  case class Lang(lang : String) extends Attribute {
    def name(which : int) : String = "lang"
    def value(which : int) : String = lang
  }

  case class Align(align : char) extends Attribute {
    def name(which : int) : String = "style"
    def value(which : int) : String = {
      align match {
	case '<' => "text-align:left";
	case '>' => "text-align:right";
	case '=' => "text-align:center";
	case '^' => "vertical-align:top";
	case '~' => "vertical-align:bottom";
	case 'f' | 'j' => "text-align:justify";
        
      }
    }
  }

  case class Em(cnt : int) extends Attribute {
    def name(which : int) : String = "style"
    def value(which : int) : String = {
      if (cnt > 0) "padding-left:"+cnt+"em" else "padding-right:"+(-cnt)+"em"
    }
  }



  case class StyleElem(name : String, value : String) extends Textile {
    def toHtml = null
    override def toString = name+":"+value
  }

  abstract class ATextile(val theElems : List[Textile], attrs : List[Attribute]) extends Textile {
    def fromStyle(st : List[Attribute]) : MetaData = {
      def toList(st : List[Attribute]) : List[Pair[String, String]] = {
        st match {
          case Nil => Nil
          case x :: xs => x.toList ::: toList(xs)
        }
      }
      
      def crunchStyle(st : List[Pair[String, String]]) : List[Pair[String,String]] = {
        val p = st.partition {a => a._1 == "style"}
        if (p._1 == Nil) p._2 else
          (p._1.reduceLeft{(a : Pair[String,String],b : Pair[String,String]) => Pair("style", a._2 + ";"+ b._2)}) :: p._2
  }
      
      def fromList(st : List[Pair[String,String]]) : MetaData = {
        st match {
          case Nil => Null
          case x :: xs => {new UnprefixedAttribute(x._1, x._2, fromList(xs))}
        }
      }
      fromList(crunchStyle(toList(st)))
    }
    
    def toHtml :NodeSeq = {
      theElems.flatMap{e => e.toHtml.toList}
    }
  }
  
  case class Line(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = super.toHtml ++ Text("\n")
  }

  case class Lst(elems : List[Textile]) extends ATextile(elems, Nil) {
    def performOnWikiAnchor(f : (WikiAnchor) => Any) : unit = {
      def findWikiAnchor(in : List[Textile], f : (WikiAnchor) => Any) : unit = {
        in match {
          case (s : WikiAnchor) :: rest => {f(s) ; findWikiAnchor(rest, f)}
          case (s : ATextile) :: rest => {findWikiAnchor(s.theElems, f); findWikiAnchor(rest, f)}
          case x :: rest => {findWikiAnchor(rest, f)}
          case _ => Nil
        }
      }
      findWikiAnchor(List(this), f)
    }
  }
  
  case class Paragraph(elems : List[Textile], attrs: List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "p", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
    }
    
  }

  case class Acronym(thing : String, acro : String) extends ATextile(Nil, Nil) {
    override def toHtml : NodeSeq = 
      Elem(null, "acronym", fromStyle(AnyAttribute("title", acro) :: Nil), TopScope, Text(thing) : _*)
  }
  
  case class Image(url : String, alt : String, link : String, attrs : List[Attribute] ) extends ATextile(Nil, attrs) {
    override def toHtml : NodeSeq = {
      val img = Elem(null, "img", fromStyle(AnyAttribute("src", url) :: AnyAttribute("alt", alt) :: attrs), TopScope, Nil : _*)
      
      if (link ne null) Elem(null, "a", fromStyle(AnyAttribute("href", link) :: attrs), TopScope, img : _*)
      else img
    }
  }

  case class BlankLine extends Textile {
    def toHtml = Text("")
  }

  case class CharBlock(s : String) extends Textile {
    def toHtml = Text(s)
  }

  case class Quoted(elems : List[Textile]) extends ATextile(elems, Nil) {
    override def toHtml : NodeSeq = {
      (Unparsed("&#8220;") ++ elems.flatMap{e => e.toHtml.toList}) ++ Unparsed("&#8221;")
    }
  }

  case class EmDash extends Textile  {
    def toHtml : NodeSeq = Unparsed(" &#8212; ")
  }

  case class EOL extends Textile  {
    // return the characters because Scala's XML library returns <br></br> in the
    // toString for the element and this causes 2 line breaks in some browsers
    // def toHtml :NodeSeq = Elem(null, "br", null, TopScope, Nil : _*) concat Text("\n")
    def toHtml :NodeSeq = Unparsed("<br />\n")
  }

  case class BOL extends Textile  {
    def toHtml : NodeSeq = Text("")
  }

  case class EnDash extends Textile  {
    def toHtml : NodeSeq = Unparsed(" &#8211; ")
  }

  case class SingleQuote extends Textile  {
    def toHtml : NodeSeq = Unparsed("&#8217;")
  }

  case class Elipsis extends Textile  {
    def toHtml : NodeSeq = Unparsed("&#8230;")
  }

  case class Dimension extends Textile   {
    def toHtml : NodeSeq = Unparsed("&#215;")
  }
  
  case class Trademark extends Textile  {
    def toHtml : NodeSeq = Unparsed("&#8482;")
  }
  case class Copyright extends Textile {
    def toHtml : NodeSeq = Unparsed("&#169;")
  }
  case class Register extends Textile {
    def toHtml : NodeSeq = Unparsed("&#174;")
  }

  case class Header(what : int, elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "h"+what, fromStyle(attrs), TopScope, super.toHtml : _*)  ++ Text("\n")
  }

  case class BlockQuote(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      val par : NodeSeq = Elem(null, "p", null, TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
      Elem(null, "blockquote", fromStyle(attrs), TopScope, par : _*)  ++ Text("\n")
    }
  }
  case class HTML(tag : String, elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, tag, fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
    }}
  case class FootnoteDef(num : String) extends ATextile(null, Nil) {
    override def toHtml : NodeSeq = {
      Elem(null, "sup", Null, TopScope, 
           Elem(null, "a", fromStyle(List(AnyAttribute("href", "#fn"+num))), TopScope, Text(num) : _*) : _*)
    }
  }

  // ) yield Footnote(reduceCharBlocks(le :: ln), attrs, (d1 :: dr).mkString("","",""))

  case class Footnote(elems : List[Textile], attrs : List[Attribute], num : String) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "p", fromStyle(AnyAttribute("id", "fn"+num) :: attrs), TopScope, 
           (Elem(null, "sup", null, TopScope, Text(num) : _*) :: elems.flatMap{e => e.toHtml.toList}) : _*) ++ Text("\n")
    }
  }

  case class Emph(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "em", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
    }
  }
  case class Strong(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "strong", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
    }
  }

  case class TableDef(attrs : List[Attribute]) extends Textile {
    def toHtml = null
  }

  case class Table(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "table", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
    }
  }

  case class TableRow(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "tr", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
    }
  }

  case class TableElement(elems : List[Textile], isHeader : boolean, attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, if (isHeader) "th" else "td", fromStyle(attrs), TopScope, (if (elems == Nil) Unparsed("&nbsp;") else elems.flatMap{e => e.toHtml.toList}) : _*) ++ Text("\n")
    }
  }

  case class Bullet(elems : List[Textile], numbered : boolean) extends ATextile(elems, Nil) {
    override def toHtml : NodeSeq = {
      Elem(null, if (numbered) "ol" else "ul", fromStyle(Nil), TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
    }
  }

  case class BulletLine(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "li", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
    }
  }

  case class Italic(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "i", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
    }
  }
  case class Bold(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = {
      Elem(null, "b", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
    }
  }
  case class Cite(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs){
    override def toHtml : NodeSeq = Elem(null, "cite", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  
  case class Code(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "code", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  
  case class Delete(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "del", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  
  case class Ins(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "ins", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  
  case class Super(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "sup", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  
  case class Sub(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "sub", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  
  case class Pre(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "pre", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*) ++ Text("\n")
  }  
  case class Span(elems : List[Textile], attrs : List[Attribute]) extends ATextile(elems, attrs) {
    override def toHtml : NodeSeq = Elem(null, "span", fromStyle(attrs), TopScope, elems.flatMap{e => e.toHtml.toList} : _*)
  }  


  case class Anchor(elems : List[Textile], href : String, alt : String, attrs : List[Attribute]) extends ATextile(elems, attrs) {
    def allAttrs = AnyAttribute("href", href) :: attrs 
    override def toHtml : NodeSeq = Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
  }

  case class RefAnchor(elems : List[Textile], ref : String, alt : String, attrs : List[Attribute]) extends ATextile(elems, attrs) {
    private var _href = ""
    def href = _href
    def href_=(i : String) {_href = i}

    def allAttrs = {AnyAttribute("href", _href) :: attrs }
    override def toHtml : NodeSeq = Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
  }

  case class ARef( name : String, href : String) extends ATextile(Nil, Nil) {
    override def toHtml : NodeSeq = Text("")
  }

  case class WikiAnchor(elems : List[Textile], href : String, alt : String, attrs : List[Attribute]) extends ATextile(elems, attrs) {
    var rootUrl = ""
    def allAttrs = AnyAttribute("href", rootUrl + href) :: attrs 
    override def toHtml : NodeSeq = Elem(null, "a", fromStyle(allAttrs), TopScope, Text(alt) : _*)
  }
  
  val example = """I am <em>very</em> serious

  Observe -- very nice!

  Observe - tiny and brief.

  "Observe!"
  
  Hello Dude

  **Bold * Not Strong**

  
  my bold line **bold**

  **strong* Not Bold


  *strong*

  This is a single paragraph
  
  This is another paragraph

  I am <b>very</b> serious.

  This
  is a paragraph

  <pre>
  I am <b>very</b> serious.
  
  Oh, yes I am!!
  </pre>
  
  I spoke.
  And none replied.
  



  Observe...
  
  Observe: 2 x 2.

  one(TM), two(R), three(C).  
  
  h1. Header 1
  second line of header 1

  h2. Header 2

  h3. Header 3

  An old text

  bq. A block quotation.

  Any old text

  This is covered elsewhere[1].
  
  fn1. Down here, in fact.

  I _believe_ every word.
  
  And then? She *fell*!
  
  I __know__.
  I **really** __know__.

  ??Cat's Cradle?? by Vonnegut  

  Convert with @r.to_html@
  
  I'm -sure- not sure.
  
  You are a +pleasant+ child.
  
  a ^2^ + b ^2^ = c ^2^
  
  log ~2~ x

  I'm %unaware% of most soft drinks.
  
  I'm %{color:red}unaware%
  of most soft drinks.

  http://hobix.com/textile/#attributes  

  I searched "Google":http://google.com.

  CamelCase

  \\CamelCase

  ThreeHumpCamel

  Four4FourHumpCamel
  

  I am crazy about "Hobix":hobix
  and "it's":hobix "all":hobix I ever
  "link to":hobix!

  [hobix]http://hobix.com

  # A first item
  # A second item
  # A third

  # Fuel could be:
  ## Coal
  ## Gasoline
  ## Electricity
  # Humans need only:
  ## Water
  ## Protein

  * A first item
  * A second item
  * A third

  * Fuel could be:
  ** Coal
  ** Gasoline
  ** Electricity
  * Humans need only:
  ** Water
  ** Protein

  | name | age | sex |
  | joan | 24 | f |
  | archie | 29 | m |
  | bella | 45 | f |

  |_. name |_. age |_. sex |
  | joan | 24 | f |
  | archie | 29 | m |
  | bella | 45 | f |

  |_. attribute list |
  |<. align left |
  |>. align right|
  |=. center |
  |<>. justify this block |
  |^. valign top |
  |~. bottom |

  |\2. spans two cols |
  | col 1 | col 2 |

  |/3. spans 3 rows | a |
  | b |
  | c |

  |{background:#ddd}. Grey cell|
  
  table{border:1px solid black}.
  |This|is|a|row|
  |This|is|a|row|
  
  |This|is|a|row|
  {background:#ddd}. |This|is|grey|row|

  p<. align left

  p>. align right

  p=. centered

  p<>. justified

  p(. left ident 1em
  
  p((. left ident 2em
  
  p))). right ident 3em

  h2()>. Bingo.
  
  h3()>[no]{color:red}. Bingo

  <pre>
  <code>
  a.gsub!( /</, '' )
  </code>
  </pre>


  <div style='float:right;'>
  float right
  </div>

  <div style='float:right;'>

  h3. Sidebar

  "Hobix":http://hobix.com/
  "Ruby":http://ruby-lang.org/

  </div>

  The main text of the
  page goes here and will 
  stay to the left of the 
  sidebar.
  
  !http://hobix.com/sample.jpg!

  !http://hobix.com/sa.jpg(Bunny.)!
  
  !http://hobix.com/sample.jpg!:http://hobix.com/

  !>http://hobix.com/sample.jpg!

  And others sat all round the small
  machine and paid it to sing to them.
  
  We use CSS(Cascading Style Sheets).
  

  """
  
}
