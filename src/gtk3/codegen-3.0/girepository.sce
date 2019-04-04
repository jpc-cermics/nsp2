
// test the gobject introspection 

G=g_irepository_get_default();
G.require["GLib","2.0"]
G.get_n_infos["GLib"]

S=G.get_shared_library["GLib"];
// GI=G.get_info["GLib",0]

GI=G.find_by_name["GLib", "assertion_message"];
GI.get_attributes[];




