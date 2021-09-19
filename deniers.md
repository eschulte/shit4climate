---
title: Deniers
layout: meta
---

{% assign sorted = collections.denier | sortByState %}
{% for denier in sorted %}
**[{{ denier.data.title }}: {{ denier.data.name }} of {{ denier.data.state }}]({{ denier.url|url }})**
:   {{ denier.data.quote }}
{% endfor %}

Collected from the [Climate Deniers 2019 -- final] Google Doc. which
was referenced from the Center for American Progress article
[Climate Deniers in the 117th Congress].

[Climate Deniers 2019 -- final]: https://docs.google.com/spreadsheets/d/1Ug_hk82Ap69aCqir0ZwVDUocyeBl6GKC6Jn-IqKeg6o/edit#gid=0
[Climate Deniers in the 117th Congress]: https://www.americanprogress.org/issues/green/news/2021/03/30/497685/climate-deniers-117th-congress/
