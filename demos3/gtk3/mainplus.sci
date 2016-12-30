


if ~exists('main_demo_gtk3','nsp-function') then 
  add_lib('NSP/demos3/gtk3/libbase');
end

if ~exists('demo_image_from_drawable','nsp-function') then 
  add_lib('NSP/demos3/gtk3/libplus');
end

demo_main_window()
