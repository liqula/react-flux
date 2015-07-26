{-# OPTIONS_GHC -cpp -pgmP cpp #-}
-- | This module contains combinators for creating DOM React elements.
--
-- The design of creating 'ReactElement's is loosly based on
-- <https://hackage.haskell.org/package/lucid lucid>.  All the combinators in this module have a
-- type
--
-- >p_ :: Term eventHandler arg result => arg -> result
--
-- but you should interpret this as 'p_' having either of the following two types:
--
-- >p_ :: ['PropertyOrHandler' eventHandler] -> 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
-- >p_ :: 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
--
-- In the first, 'p_' takes a list of properties and handlers plus the child element(s).  In the
-- second, the list of properties and handlers is omitted. The 'Term' class allows GHC to
-- automatically select the appropriate type.
--
-- Properties are built using '(@=)'.  Be aware that in React, there are some
-- <https://facebook.github.io/react/docs/dom-differences.html differences> between the browser DOM
-- objects/properties and the properties and attributes you pass to React, as well as React only
-- supports  <https://facebook.github.io/react/docs/tags-and-attributes.html certian attributes>.
-- Event handlers can be created by the combinators in "React.Flux.Events".
--
-- Elements not covered by this module can be created manually using 'el'.  But be aware that React
-- only supports <https://facebook.github.io/react/docs/tags-and-attributes.html certian elements>
-- and they should all be covered by this module.
--
-- TODO
module React.Flux.DOM where

import React.Flux.Element
import React.Flux.PropertiesAndEvents

class Term eventHandler arg result | result -> arg, result -> eventHandler where
    term :: String -> arg -> result

instance (child ~ ReactElementM eventHandler a) => Term eventHandler [PropertyOrHandler eventHandler] (child -> ReactElementM eventHandler a) where
    term name props = el name props

instance Term eventHandler (ReactElementM eventHandler a) (ReactElementM eventHandler a) where
    term name child = el name [] child

#define elem(name) name ## _ :: Term eventHandler arg result => arg -> result; name ## _ = term #name

-- Copy the elements from react documentation and use :s/ /)^v^Melem(/g

-- HTML

elem(a)
elem(abbr)
elem(address)
elem(area)
elem(article)
elem(aside)
elem(audio)
elem(b)
elem(base)
elem(bdi)
elem(bdo)
elem(big)
elem(blockquote)
elem(body)
elem(br)
elem(button)
elem(canvas)
elem(caption)
elem(cite)
elem(code)
elem(col)
elem(colgroup)
elem(data)
elem(datalist)
elem(dd)
elem(del)
elem(details)
elem(dfn)
elem(dialog)
elem(div)
elem(dl)
elem(dt)
elem(em)
elem(embed)
elem(fieldset)
elem(figcaption)
elem(figure)
elem(footer)
elem(form)
elem(h1)
elem(h2)
elem(h3)
elem(h4)
elem(h5)
elem(h6)
elem(head)
elem(header)
elem(hr)
elem(html)
elem(i)
elem(iframe)
elem(img)
elem(input)
elem(ins)
elem(kbd)
elem(keygen)
elem(label)
elem(legend)
elem(li)
elem(link)
elem(main)
elem(map)
elem(mark)
elem(menu)
elem(menuitem)
elem(meta)
elem(meter)
elem(nav)
elem(noscript)
elem(object)
elem(ol)
elem(optgroup)
elem(option)
elem(output)
elem(p)
elem(param)
elem(picture)
elem(pre)
elem(progress)
elem(q)
elem(rp)
elem(rt)
elem(ruby)
elem(s)
elem(samp)
elem(script)
elem(section)
elem(select)
elem(small)
elem(source)
elem(span)
elem(strong)
elem(style)
elem(sub)
elem(summary)
elem(sup)
elem(table)
elem(tbody)
elem(td)
elem(textarea)
elem(tfoot)
elem(th)
elem(thead)
elem(time)
elem(title)
elem(tr)
elem(track)
elem(u)
elem(ul)
elem(var)
elem(video)
elem(wbr)


-- SVG

elem(circle)
elem(clipPath)
elem(defs)
elem(ellipse)
elem(g)
elem(line)
elem(linearGradient)
elem(mask)
elem(path)
elem(pattern)
elem(polygon)
elem(polyline)
elem(radialGradient)
elem(rect)
elem(stop)
elem(svg)
elem(text)
elem(tspan)
