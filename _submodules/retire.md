```shell
join -1 1 -2 1 -o 1.1,1.2 \
  <(cat _submodules/senators.txt|grep Republican|cut -f1,2|sed 's/\(.*\), \(.*\)\t\(.*\)$/\3,\2 \1/'|tr '[A-Z]' '[a-z]'|sed 's/ /-/g;s/,/ /g'|sort) \
  <(cat _submodules/governers.txt|grep Democratic|cut -f1,2|tr '\t' ','|sed 's/Governor of //'|tr '[A-Z]' '[a-z]'|sed 's/ /-/g;s/,/ /g'|sort)
```

| State          | Name             | Phone        |
|----------------|------------------|--------------|
| Kansas         | Jerry Moran      | 202-224-6521 |
| Kansas         | Roger Marshall   | 202-224-4774 |
| Kentucky       | Mitch Mcconnell  | 202-224-2541 |
| Kentucky       | Rand Paul        | 202-224-4343 |
| Louisiana      | Bill Cassidy     | 217-782-8088 |
| Louisiana      | John Kennedy     | 202-224-4623 |
| Maine          | Susan M Collins  | 202-224-2523 |
| North Carolina | Richard burr     | 202-224-3154 |
| North Carolina | Thom tillis      | 202-224-6342 |
| Pennsylvania   | Patrick J Toomey | 202-224-4254 |
| Wisconsin      | Ron Johnson      | 202-224-5323 |
