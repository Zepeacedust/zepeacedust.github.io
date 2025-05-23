---
title: "Making of: The library of Durenmar"
date: 26/3/2025
abstract: An overview of the creation of a bot for fetching spells on the ars magica discord server.
tags:
    - coding
    - ArsMagica
---
This is not a techincal dissection, more of a narrative of the development of a pet project.
It is relayed accurately, if you brush off the analogies of fighting mythic beasts.
For the code itself see the [Github](https://github.com/Zepeacedust/AutoLibrarian).

Like most projects, this one started as a minor annoyance in conversation about something petty.
That initial conversation went something like this:

> A: Why does this work?
>
> B: By analogy to The Patient Spell
>
> A: What's the text on that again?

Then I had an idea, what if it had went something like this:

> A: Why does this work?
>
> B: By analogy to [[The Patient Spell]]

And it would just link the spell automatically.

Now there's a thought.

Problem was: there is no accesible dataset that encompasses all the published spells.
But there is a pdf that contains them all.
So late at night I ripped the text from the pdf and set to work parsing it into something usable.

I bring my strongest regular expression down on the beast, and come 2 past midnight I was satisfied with my handiwork. I had no intents of writing the fetcher itself soon anyways.

Around noon the day after I am locked in mortal combat with [Aeson](https://hackage.haskell.org/package/aeson), a json parsing library that had promised much and delivered ~~fire and brimstone~~ obtuse errors and terse documentation.

On its left was a beast of great power, wrought by my own hand, 6000 lines of json made by regular expressions and a sleep-deprived mind set on creation. And on its right was an error message of cold, austere beauty, Nothing at all.

I took my old axe by the handle and started hacking, splitting the problem in twain until I found the solid core of error inside, when I found that error I analized it and forged a regular expression to kill it and its kind.
One after the other I cut down the issues before me, heedless of the mountains of minor issues left behind.
In my rage I had clobbered the spell levels, and homoginized the formatting, they were not semantically wrong so they must be correct.

After I had parsed the beast, I built with its bones a spell fetcher, it showed the name, parameters, text and source of the requested spell.
Behind the veneer of progress, I had shattered the foundations. This was the only json of its kind, should it not be good and legible?
Should it not be accurate and precise?
Is it not my duty to un-clobber the spell levels?

From the mother of the beast I ripped a new text file, and armed with the weapons honed in the blood of its sibling I hacked this one into neater pieces. Aware of the errors of the past I made a json file, 10000 lines long, shining and beautiful.

[Aeson](https://hackage.haskell.org/package/aeson) had been conquered, and the new parser was written over the mistakes of the past. A pure dataset leads to pure outputs.

The requested spells were extracted with a regular expression, and sought for in the parsed json.
Any matches were sent.

But some were too big, at over 3500 characters the Aegis of the Hearth was too big to fit into a discord message. So any messages over a threshold were to be sent as a file instead (Sidenote: Why does discord not have native foldable message support?).

The masses rejoiced, but there was trouble in paradise.

> A: Fuck you and your Covenant, I cast [[Stripping the Superfluous Reality]]
>
> B: Spell not found
>
> A: Fuck

The masses, regrettably were not as perfect as the dataset, and what is a library that you cannot find the correct book in?
I scrambled to the archives, and found an implementation of an edit distance algorithm, [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance). Instead of finding a perfect match, it selects the closest spell available.
For short messages, this can get quite nonsensical, but it compensates for fairly major spelling differences in a reasonable timescale.

This improved system led to a strange behaviour in the gathered wizard impersonators: They saw it as an oracle, asking to find them names and interpreting the resulting spells.

My beautiful machine now rests on a raspberry pi on a desk, faithfully returning spells and hopefully making conversations about a game about pretending to be wizards just that bit more convenient.

The feeling I get seeing someone enjoy something I made is next to none, the pride in seeing your creation work and meed or exceed expectations is intoxicating, I should share more things that I make.
I guess that's what this blog is for, stay tuned for more nonsense.
