## Motivation
In some scenario of stream computation, the raw data are usually collected from a message queue like kafka，which will be persistented for further analyze. For most of the time, the raw data are store as a simple java bean in memory and could easily be converted to a Map. The scala_sql is designed to run simple SQL on the data structure  List&lt;Map>, allowing the programmer to do some real time computing.

### Demo
suppose there is a List<Map> called user,which contains these data:  
[name:tsc,age:30,sex:male]  
[name:syy,age:29,sex:female]  
[name:dudu,age:1,sex:male]  
[name:xiaohua,age:2,sex:null]  
[name:tsc,age:19,sex:male]  
[name:tsc,age:99,sex:female]  
[name:tsc,age:30,sex:female]  

 then
```scala
Engine.query(user,"select count(user) as number,name from user group by name")
```  
will result in：  
[number:1,name:dudu]  
[number:4,name:tsc]  
[number:1,name:xiaohua]  
[number:1,name:syy]  

### supported function and clause
max,min,sum,count,count(distinct),avg
order by, group by, limit

### supported java/scala data type
String,Integer,Double,Date

### need to improve
1.currently the java/scala data type are implicitly converted to the build-in type using
the scala implicit class, however it can't not convert null which is a special type in
java, so when doing computation like sum,order by on column which has a null value, the
code will throw an exception. In another word, if a column has a null value, you can not
do computation on it.

2.The engine uses the build in function
```scala
sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repr = sorted(ord on f)
```
to do order by, as the engine do not support clause like
```sql
order by a desc, b asc
```
