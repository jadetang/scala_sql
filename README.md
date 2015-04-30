#Motivation
In some scenario of stream computation, the raw data are usually collected from a message queue like kafka，which will be persistented for further analyze. For most of the time, the raw data are store as a simple java bean in memory and could easily be converted to a Map. The scala_sql is designed to run simple SQL on the data structure  List&lt;Map>, allowing the programmer to do some real time computing.

#Demo
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
Engine.query(user,"select count(user) as number,name from user group by name") will result in
```
[number:1,name:dudu]  
[number:4,name:tsc]  
[number:1,name:xiaohua]  
[number:1,name:syy]  
