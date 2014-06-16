// Console Populate:
// kvs_add({type:"user", id:"1",container:"feed",name:"maxim"});
// kvs_add({type:"user", id:"2",container:"feed",name:"maxim"});
// kvs_add({type:"user", id:"3",container:"feed",name:"maxim"});

// localStorage:
// feeduser : {"type":"feed","id":"user","count":3,"top":"3"}
// user1 : {"type":"user","id":"1","container":"feed","name":"maxim","next":"2","feed_id":"user"}
// user2 : {"type":"user","id":"2","container":"feed","name":"maxim","prev":"1","next":"3","feed_id":"user"}
// user3 : {"type":"user","id":"3","container":"feed","name":"maxim","prev":"2","next":null,"feed_id":"user"}

// Console Traverse:
// kvs_read("user","1",-1);
// [Object,Object,Object]

function kvs_put(o) { localStorage.setItem(o.type+o.id, JSON.stringify(o)); }

function kvs_add(iterator) {
    var type = iterator.type;
    var id = iterator.id;
    var local = localStorage.getItem(type+id);
    if (null == local) {
        var container_name = iterator.container;
        var feed_id = iterator.feed_id ? iterator.feed_id : iterator.type;
        var container = localStorage.getItem(container_name+feed_id);
        if (null == container) {
            container = {type: iterator.container, id: feed_id, count: 0 };
            kvs_put(container);
        } else container = JSON.parse(container);
        var prevTop = container.top;
        var prev = localStorage.getItem(type+prevTop);
        if (null != prev) {
            prev = JSON.parse(prev);
            prev.next = id;
            kvs_put(prev);
            iterator.prev = prev.id;
        }
        container.top = iterator.id;
        container.count++;
        iterator.next = null;
        iterator.feed_id = feed_id;
        kvs_put(container);
        kvs_put(iterator);
    }
}

function kvs_read(type,start,count) { return traversal(type,start,count,[]); }
function traversal(type,start,count,result) {
    var item = localStorage.getItem(type+start);
    if (null == item || count == 0) return result; else {
        item = JSON.parse(item);
        result.push(item);
        if (count <= result.length && count != -1) return result; else
        return traversal(type,item.next,count,result);
    }
}
