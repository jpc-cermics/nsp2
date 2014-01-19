// -*- Mode:scilab -*- 

A=bson_create(hash(5,x=78,b=78,d=89,e=6,x8="pipo",foo=%t))
A.show[]
A.to_hash[]

// open connection to mongodb 
cl=mclient_create('mongodb://127.0.0.1/');
// get a collection testData in base "test" 
col= cl.get_collection["test","testData"];
// query the collection to get a cursor 

if %f then 
  A=bson_create(hash(5,x="un"));
  col.insert[A];
end

// cursor.more[] renvoit False trop vite !

cursor= col.find[];
R={};
while %t then 
  E= cursor.next[];
  if type(E,'short')== 'b' then break;end
  R{$+1}= E.to_hash[];
end
// release the cursor;
cursor=[];

// delete elements in a collection 

col= cl.get_collection["test","testData"];

cursor= col.find[];
R={};
while %t then 
  E= cursor.next[];
  if type(E,'short')== 'b' then break;end
  R{$+1}= E.to_hash[];
end

col.delete[]; // delete evrything in testData 

// fill testData 
for i=1:100 
  A=bson_create(hash(5,x=sprintf("%s%d","s",i)));
  col.insert[A];
end
// check 

cursor= col.find[];
R={};
while %t then 
  E= cursor.next[];
  if type(E,'short')== 'b' then break;end
  R{$+1}= E.to_hash[];
end



