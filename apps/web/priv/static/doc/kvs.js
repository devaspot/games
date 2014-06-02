
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
