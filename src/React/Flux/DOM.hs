{-# OPTIONS_GHC -cpp -pgmP cpp #-}

-- | This module contains combinators for creating DOM React elements.
--
-- The design of creating 'ReactElement's is loosly based on
-- <https://hackage.haskell.org/package/lucid lucid>.
-- Most of the combinators in this module have a type:
--
-- @
-- p_ :: 'Term' eventHandler arg result => arg -> result
-- @
--
-- but you should interpret this as 'p_' having either of the following two types:
--
-- @
-- p_ :: ['PropertyOrHandler' eventHandler] -> 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
-- p_ :: 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
-- @
--
-- In the first, 'p_' takes a list of properties and handlers plus the child element(s).  In the
-- second, the list of properties and handlers is omitted. The 'Term' class allows GHC to
-- automatically select the appropriate type.
--
-- Be aware that in React, there are some
-- <https://facebook.github.io/react/docs/dom-differences.html differences> between the browser DOM
-- objects/properties and the properties and attributes you pass to React, as well as React only
-- supports  <https://facebook.github.io/react/docs/tags-and-attributes.html certian attributes>.
-- Event handlers can be created by the combinators in "React.Flux.PropertiesAndEvents".
--
-- Elements not covered by this module can be created manually using 'el'.  But React
-- only supports <https://facebook.github.io/react/docs/tags-and-attributes.html certian elements>
-- and they should all be covered by this module.
--
-- For example,
--
-- >ul_ $ do li_ (b_ "Hello")
-- >         li_ "World"
-- >         li_ $
-- >             ul_ (li_ "Nested" <> li_ "List")
--
-- would build something like
--
-- ><ul>
-- >  <li><b>Hello</b><li>
-- >  <li>World</li>
-- >  <li><ul>
-- >    <li>Nested</li>
-- >    <li>List</li>
-- >  </ul></li>
-- ></ul>
module React.Flux.DOM where

import React.Flux.Internal

-- | This class allows the DOM combinators to optionally take a list of properties or handlers, or
-- for the list to be omitted.
class Term eventHandler arg result | result -> arg, result -> eventHandler where
    term :: String -> arg -> result

instance (child ~ ReactElementM eventHandler a) => Term eventHandler [PropertyOrHandler eventHandler] (child -> ReactElementM eventHandler a) where
    term name props = el name props

instance Term eventHandler (ReactElementM eventHandler a) (ReactElementM eventHandler a) where
    term name child = el name [] child

#define node(name) name ## _ :: Term eventHandler arg result => arg -> result; name ## _ = term #name
#define elem(name) name ## _ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); name ## _ p = el #name p mempty

-- Copy the elements from react documentation and use :s/ /)^v^Mnode(/g

-- HTML

node(a)
node(abbr)
node(address)
node(area)
node(article)
node(aside)
node(audio)
node(b)
node(base)
node(bdi)
node(bdo)
node(big)
node(blockquote)
node(body)
elem(br)
node(button)
node(canvas)
node(caption)
node(cite)
node(code)
node(col)
node(colgroup)
node(data)
node(datalist)
node(dd)
node(del)
node(details)
node(dfn)
node(dialog)
node(div)
node(dl)
node(dt)
node(em)
node(embed)
node(fieldset)
node(figcaption)
node(figure)
node(footer)
node(form)
node(h1)
node(h2)
node(h3)
node(h4)
node(h5)
node(h6)
node(head)
node(header)
elem(hr)
node(html)
node(i)
node(iframe)
node(img)
elem(input)
node(ins)
node(kbd)
node(keygen)
node(label)
node(legend)
node(li)
node(link)
node(main)
node(map)
node(mark)
node(menu)
node(menuitem)
node(meta)
node(meter)
node(nav)
node(noscript)
node(object)
node(ol)
node(optgroup)
node(option)
node(output)
node(p)
node(param)
node(picture)
node(pre)
node(progress)
node(q)
node(rp)
node(rt)
node(ruby)
node(s)
node(samp)
node(script)
node(section)
node(select)
node(small)
node(source)
node(span)
node(strong)
node(style)
node(sub)
node(summary)
node(sup)
node(table)
node(tbody)
node(td)
node(textarea)
node(tfoot)
node(th)
node(thead)
node(time)
node(title)
node(tr)
node(track)
node(u)
node(ul)
node(var)
node(video)
node(wbr)


-- SVG

node(circle)
node(clipPath)
node(defs)
node(ellipse)
node(g)
node(line)
node(linearGradient)
node(mask)
node(path)
node(pattern)
node(polygon)
node(polyline)
node(radialGradient)
node(rect)
node(stop)
node(svg)
node(text)
node(tspan)
