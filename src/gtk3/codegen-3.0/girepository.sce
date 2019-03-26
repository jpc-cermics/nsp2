
// test the gobject introspection 

G=g_irepository_get_default();
G.require["GLib","2.0"]
G.get_n_infos["GLib"]

GI=G.find_by_name["GLib", "assertion_message"];
