---
title: "interpretation"
date: "12/02/2021"
output: html_document
runtime: shiny
---
***

#### How do you interpret these results?
The Bayes factor shows how much more or less likely your data are under the alternative hypothesis---which you specified on the left---compared with a null hypothesis of zero effect. 

For example, a Bayes factor of 50 means your data were fifty times  *more* likely under the alternative hypothesis than the null, which provides strong relative evidence for there being an effect. Conversely, a Bayes factor of 1/50 (0.02) means your data were fifty times  *less* likely under the alternative hypothesis than the null, which provides strong relative evidence for no effect. Finally, a Bayes factor of 1 shows your data were *equally* likely under both the alternative and null hypotheses, so they provide no relative evidence at all.


While Bayes factors are [continuous measures of evidence](https://psyarxiv.com/bua5n/), the following conventions can be used for journals that require p-values of <.05:^[1] 


> <span style="color:green">**Evidence for effect:**</span> A Bayes factor greater than 3 indicates moderate relative evidence for an effect.

> <span style="color:red">**Evidence for no effect:**</span> A Bayes factor less than 1/3 indicates moderate relative evidence for no effect.

> <span style="color:orange">**Inconclusive evidence:**</span> A Bayes factor between 1/3 and 3 indicates inconclusive evidence.

In addition, Bayes factors of greater than 10 provide strong evidence for an effect, whereas Bayes factors of less than 1/10 provide strong evidence for no effect. See [Dienes 2014](https://psyarxiv.com/yc7s5/) for more information on how to interpret and report Bayes factors.

***

#### Why use Bayes factors?

The Bayes factor tells you how much more likely your data are under one hypothesis over another. It is the Bayesian equivalent of significance testing. Importantly, it answers a question that p-values from null-hypothesis significance tests cannot: 

> Do my data provide evidence for no effect?

Bayes factors allow you to determine whether your data (i) provide evidence for an effect, (ii) provide [evidence for no effect](https://www.frontiersin.org/articles/10.3389/fpsyg.2014.00781/full), or (iii) are inconclusive. 

P-values do not allow this, as they conflate evidence for no effect and inconclusive evidence. A non-significant p-value therefore does not prove there is no effect, [despite many scientists believing they do](https://doi.org/10.1038/d41586-019-00857-9). In fact, it does not even imply you should reduce your belief in the hypothesis you are testing. To determine how your beliefs should change, you must first calculate a Bayes factor.^[2]



***

^[1] This convention was first suggested by [Jeffreys in 1939](https://scholar.google.com/scholar_lookup?title=Theory+of+probability&author=H+Jeffreys&publication_year=1939&), and is [useful](https://psyarxiv.com/bua5n/) for journals that use a convention of p<.05. However, there is no deterministic relationship between Bayes factors and p-values.

^[2] For other solutions to this problem, see  [Lakens et al. 2018](https://psyarxiv.com/qtzwr/).

***