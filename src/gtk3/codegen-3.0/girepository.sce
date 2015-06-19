
// test the gobject introspection 

G=g_irepository_get_default();
G.require["GLib","2.0"]
G.get_infos["GLib"]
