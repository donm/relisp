#--
# Copyright (C) 2009 <don@ohspite.net>
#
# This file is part of Relisp.
#
# Relisp is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#  
# Relisp is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.
#++
#
# See the documentation in <tt>programming_types.rb</tt>.
#
# Elisp editing types as described in the elisp info manual:
#
#   Editing Types
#
#   Buffer Type::         The basic object of editing.
#   Marker Type::         A position in a buffer.
#   Window Type::         Buffers are displayed in windows.
#   Frame Type::	    Windows subdivide frames.
#   Window Configuration Type::   Recording the way a frame is subdivided.
#   Frame Configuration Type::    Recording the status of all frames.
#   Process Type::        A process running on the underlying OS.
#   Stream Type::         Receive or send characters.
#   Keymap Type::         What function a keystroke invokes.
#   Overlay Type::        How an overlay is represented.  
#
# There are currently no Relisp classes for frame configurations,
# streams, and keymaps.  Frame configurations and keymaps are just
# +cons+ with a certain keyword in the +car+.  Streams are markers,
# buffers, strings, and functions.
#
# Unlike with elisp programming data types, ruby does not translate
# the editing data types to an equivalent ruby object; instead, the
# class corresponding to each data type is just a proxy that acts on
# the original object in elisp.

module Relisp

  # Proxy contains the code that creates a wrapper around a variable
  # in emacs.
  #
  class Proxy
    def self.from_elisp(object)
      new(object[:variable], object[:slave])
    end

    # If the last argument is a Relisp::Slave, it gets pulled off and
    # used as the slave; otherwise Relisp.default_slave is used.  If
    # the first argument is a Symbol, it is assumed to be the name of
    # an elisp variable which needs a proxy.  If the first argument
    # isn't a Symbol, all of the arguments (except the last, if it was
    # a Slave) are send off to the child to handle.
    #
    def initialize(*args)
      @slave = if args.last.kind_of?(Relisp::Slave)
                 args.pop
               else
                 Relisp.default_slave
               end

      if args[0].kind_of?(Symbol) && args[1].nil?
        @elisp_variable = @slave.get_permanent_variable(args[0])
        elisp_type= ""
        self.class.to_s.split("::").last.split(//).each_with_index do |char, index|
          unless index==0 || char == char.downcase
            elisp_type << "-"
          end
          elisp_type << char.downcase
        end

        unless @slave.elisp_eval("(type-of #{@elisp_variable})") == elisp_type.to_sym
          raise ArgumentError, "#{@elisp_variable} isn't a #{elisp_type}"
        end
      else
        @elisp_variable = @slave.new_elisp_variable
        yield args
      end
    end
  
    attr_reader :slave, :elisp_variable

    def to_elisp
      @elisp_variable
    end

    private 
    
    def call_on_self(function, *args)
      @slave.send(function, @elisp_variable.value, *args)
    end
  end

  # A proxy to an Emacs buffer.
  #
  class Buffer < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (_string_, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing bufer in the _slave_ process.  Otherwise a new
    # buffer is created (<tt>generate-new-buffer</tt>).  The name is
    # _string_, if given, and a variant of "relisp-buffer" otherwise.
    #
    def initialize(*args)
      super do |args|
        name = args[0] ? args[0] : "relisp-buffer"
        raise ArgumentError unless name.kind_of?(String)
        @slave.elisp_exec( "(setq #{@elisp_variable} (generate-new-buffer #{name.to_elisp}))" )
      end
    end

    private

    def eval_in_buffer(code)
      @slave.elisp_eval <<-EOM
         (save-excursion
           (set-buffer #{@elisp_variable})
           #{code})
       EOM
    end

    public

    def set
      @slave.set_buffer(@elisp_variable.value)
    end
    
    # Return the name of Buffer, as a string (<tt>buffer-name</tt>).
    #
    def name
      call_on_self :buffer_name
    end
 
    # Change current buffer's name to _newname_ (a string).  If
    # _unique_ is nil, it is an error if a buffer named _newname_
    # already exists.  If _unique_ is non-nil, come up with a new name
    # using <tt>generate-new-buffer-name'</tt>
    # (<tt>rename-buffer</tt>).
    #
    def rename(newname, unique = false)
      eval_in_buffer "(rename-buffer #{newname.to_elisp} #{unique.to_elisp})"
    end

    # Return name of file that the Buffer is visiting, or nil if none
    # (<tt>buffer-file-name</tt>).
    #
    def filename
      call_on_self :buffer_file_name
    end

    # Change name of file visited in the Buffer to _newname_.  This
    # also renames the buffer to correspond to the new file.  The next
    # time the buffer is saved it will go in the newly specified file.
    # _newname_ nil or an empty string means mark buffer as not
    # visiting any file.
    #
    # The optional <em>along_with_file</em>, if non-nil, means that
    # the old visited file has been renamed to _newname_
    # (<tt>set-visited-file-name</tt>).
    #
    def filename=(newname, along_with_file=false)
      # the second argument here inhibits confirmation in the case
      # where another buffer is already visiting _newname_.
      eval_in_buffer "(set-visited-file-name #{newname.to_elisp} t #{along_with_file.to_elisp})"
    end

    def modified?
      call_on_self :buffer_modified_p
    end

    def set_modified(flag=true)
      eval_in_buffer "(set-buffer-modified-p #{flag.to_elisp})"
    end

    def modified=(flag)
      set_modified(flag)
    end

    def modified_tick
      call_on_self :buffer_modified_tick
    end

    def chars_modified_tick
      call_on_self :buffer_chars_modified_tick
    end

    def read_only?
      eval_in_buffer "buffer-read-only"
    end

    def read_only=(flag)
      eval_in_buffer "(setq buffer-read-only #{flag.to_elisp})"
    end

    def bury
      call_on_self :bury_buffer
    end

    def kill
      raise if modified?
      call_on_self :kill_buffer
    end

    def kill!
      set_modified(false)
      kill
    end

    def alive?
      call_on_self :buffer_live_p
    end

    # Save the buffer in its visited file, if it has been modified
    # (<tt>save-buffer</tt>).
    #
    def save
      raise "Attempt to save buffer with no filename." unless filename
      eval_in_buffer "(with-output-to-string (save-buffer))"
    end
    
    def write(newfile)
      eval_in_buffer "(with-output-to-string (write-file #{newfile.to_elisp}))"
    end

    def size
      call_on_self :buffer_size
    end

    def substring(start_position, end_position)
      eval_in_buffer "(buffer-substring #{start_position} #{end_position})"
    end

    def substring_no_properties(start_position, end_position)
      eval_in_buffer "(buffer-substring-no-properties #{start_position} #{end_position})"
    end

    def to_s
      eval_in_buffer "(buffer-string)"
    end

    def erase
      eval_in_buffer "(erase-buffer)"      
    end

    def window
      call_on_self :get_buffer_window
    end

    def window=(new_window)
      new_window.buffer = self
    end

    def insert(object)
      eval_in_buffer "(insert #{object.to_elisp})"
    end

    alias print insert

    def puts(object="")
      line_number = @slave.line_number_at_pos
      insert object
      if line_number == @slave.line_number_at_pos
        insert "\n"
      end
    end

    def <<(object)
      @slave.save_excursion do 
        set
        @slave.elisp_eval "(goto-char (point-max))"
        insert object
      end
      return self
    end

    def method_missing(method, *args)
      @slave.save_excursion do 
        set
        @slave.send(method, *args)
      end
    end
  end

  # A  proxy to an Emacs marker.
  #
  class Marker < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing marker in the _slave_ process.  Otherwise a new,
    # empty marker is created (<tt>make-marker</tt>).
    #
    def initialize(*args)
      super do 
        @slave.elisp_exec( "(setq #{@elisp_variable} (make-marker))" )
      end
    end

    def position
      call_on_self :marker_position
    end

    def buffer
      call_on_self :marker_buffer
    end

    def insertion_type
      call_on_self :marker_insertion_type
    end

    def insertion_type=(type = true)
      @slave.elisp_eval( "(set-marker-insertion-type #{@elisp_variable} #{type.to_elisp})" )
    end

    def set(new_position, new_buffer=nil)
      @slave.elisp_eval( "(set-marker #{@elisp_variable} #{new_position} #{new_buffer})" )
    end

    alias move set
    alias position= set
  end

  # A  proxy to an Emacs window
  #
  class Window < Proxy

    # _args_ must be of the form (_symbol_, <em>slave =
    # Relisp.default_slave</em>)
    #
    # The _symbol_ argument is considered to be the name of a
    # pre-existing window in the _slave_ process.  
    #
    def initialize(*args)
      super do 
        raise ArgumentError, "Cannot create Window using 'new' method."
      end
    end

    private

    def eval_in_window(code)
      @slave.elisp_eval <<-EOM
         (with-selected-window #{@elisp_variable}
           #{code})
       EOM
    end

    public

    def split(size=nil, horizontal=false)
      call_on_self :split_window, size, horizontal
    end

    def alive?
      call_on_self :window_live_p
    end

    def delete
      call_on_self :delete_window
    end

    def delete_others
      call_on_self :delete_other_windows
    end

    def select
      call_on_self :select_window
    end

    def buffer
      call_on_self :window_buffer
    end

    def buffer=(new_buffer)
      call_on_self :set_window_buffer, new_buffer
    end

    def dedicated
      call_on_self :window_dedicated_p
    end

    def dedicated=(flag)
      call_on_self :set_window_dedicated_p, flag
    end

    def point
      call_on_self :window_point
    end

    def point=(position)
      call_on_self :set_window_point, position
    end

    def start
      call_on_self :window_start
    end

    def start=(position)
      call_on_self :set_window_start, position
    end

    def end 
      call_on_self :window_end
    end

    def visible?(position)
      @slave.elisp_eval( "(pos-visible-in-window-p #{position.to_elisp} #{@elisp_variable})" )
    end

    def scroll_up(count=nil)
      eval_in_window "(scroll-up #{count.to_elisp})"
    end

    def scroll_down(count=nil)
      eval_in_window "(scroll-down #{count.to_elisp})"
    end

    def recenter(count=nil)
      eval_in_window "(recenter #{count.to_elisp})"
    end

    def vscroll
      call_on_self :window_vscroll
    end

    def vscroll_in_pixels
      call_on_self :window_vscroll, true
    end

    def vscroll=(lines)
      call_on_self :set_window_vscroll, lines
    end

    def vscroll_in_pixels=(lines)
      call_on_self :set_window_vscroll, lines, true
    end

    def scroll_left(count=nil)
      eval_in_window "(scroll-left #{count.to_elisp})"
    end

    def scroll_right(count=nil)
      eval_in_window "(scroll-right #{count.to_elisp})"
    end
    
    def hscroll
      call_on_self :window_hscroll
    end

    def hscroll=(columns)
      call_on_self :set_window_hscroll, columns
    end

    def height
      call_on_self :window_height
    end

    def body_height
      call_on_self :window_body_height
    end

    def width
      call_on_self :window_width
    end

    def edges
      (call_on_self :window_edges).to_list
    end

    def inside_edges
      (call_on_self :window_inside_edges).to_list
    end

    def pixel_edges
      (call_on_self :window_pixel_edges).to_list
    end

    def inside_pixel_edges
      (call_on_self :window_inside_pixel_edges).to_list
    end

    def enlarge(vertical, horizontal=0)
      eval_in_window "(enlarge-window #{vertical.to_elisp})"
      eval_in_window "(enlarge-window-horizontally #{horizontal.to_elisp})"
    end

    def shrink(vertical, horizontal=0)
      eval_in_window "(shrink-window #{vertical.to_elisp})"
      eval_in_window "(shrink-window-horizontally #{horizontal.to_elisp})"
    end

    def frame
      call_on_self :window_frame
    end

      
  end

  # A proxy to an Emacs frame
  #
  class Frame < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (<em>option_hash = {}</em>, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing frame in the _slave_ process.  Otherwise a new
    # frame is created using any options in (<tt>new-frame</tt>).
    #
    # The _option_hash_ can specify the following
    # [<tt>:name =></tt> _string_] 
    #      The frame should be named _string_.
    # [<tt>:width =></tt> _fixnum_] 
    #      The frame should be _fixnum_ characters in width.
    # [<tt>:height =></tt> _fixnum_] 
    #      The frame should be _fixnum_ text lines high.
    #
    # You cannot specify either :width or :height, you must use neither or both.
    #
    # [<tt>:minibuffer => true</tt>]
    #      The frame should have a minibuffer.
    # [<tt>:minibuffer => nil</tt>]
    #      The frame should have no minibuffer.
    # [<tt>:minibuffer => :only</tt>] 
    #      The frame should contain only a minibuffer.
    # [<tt>:minibuffer =></tt> _window_] 
    #      The frame should use _window_ as its minibuffer window.
    #
    # [<tt>:"window-system" => nil</tt>] 
    #      The frame should be displayed on a terminal device.
    # [<tt>:"window-system" => :x</tt>] 
    #      The frame should be displayed in an X window.
    # [<tt>:terminal =></tt> _id_] 
    #      The frame should use the terminal identified by _id_.
    #
    def initialize(*args)
      super do |args|
        hash = args[0]
        alist = ""
        if hash && hash.size > 1
          alist << "'("
          hash.each_pair do |key, val|
            val = if val.kind_of?(Symbol)
                    val.value.to_elisp
                  else
                    val.to_elisp
                  end
            alist << "(#{key} . #{val.to_s}) "
          end
          alist << ")"
        end

        @slave.elisp_exec( "(setq #{@elisp_variable} (make-frame #{alist}))" )
      end
    end

    def delete
      call_on_self :delete_frame
    end

    def alive?
      call_on_self :frame_live_p
    end

    def selected_window
      call_on_self :frame_selected_window
    end

    def selected_window=(window)
      call_on_self :set_frame_selected_window, window
    end
    
    def focus
      call_on_self :select_frame_set_input_focus
    end

    def select
      call_on_self :select_frame
    end

    def visible?
      call_on_self :frame_visible_p
    end

    def show
      call_on_self :make_frame_visible
    end

    def hide
      call_on_self :make_frame_invisible
    end

    def iconify
      call_on_self :inconify_frame
    end

    def raise
      call_on_self :raise_frame
    end

    def lower
      call_on_self :lower_frame
    end

    private
    
    def get_parameter(parameter)
      call_on_self :frame_parameter, parameter
    end

    def set_parameter(parameter, new_value)
      alist = Cons.new(Cons.new(parameter, new_value), nil)
      call_on_self :modify_frame_parameters, alist
    end

    public

    ######### begin frame parameters

    #
    def display
      get_parameter :display
    end

    def display=(new_value)
      set_parameter :display, new_value
    end

    def display_type
      get_parameter :display_type
    end

    def display_type=(new_value)
      set_parameter :display_type, new_value
    end

    def title
      get_parameter :title
    end

    def title=(new_value)
      set_parameter :title, new_value
    end

    def name
      get_parameter :name, new_value
    end

    def name=(new_value)
      set_parameter :name, new_value
    end

    def left
      get_parameter :left
    end

    def left=(new_value)
      set_parameter :left, new_value
    end

    def top
      get_parameter :top
    end
    
    def top=(new_value)
      set_parameter :top, new_value
    end

    def height
      get_parameter :height
    end
    
    def height=(new_value)
      set_parameter :height, new_value
    end

    def width
      get_parameter :width
    end

    def width=(new_value)
      set_parameter :width, new_value
    end

    def fullscreen
      get_parameter :width      
    end

    def fullscreen=(new_value)
      set_parameter :width, new_value
    end

    def border_width
      get_parameter :border_width
    end

    def border_width=(new_value)
      set_parameter :border_width, new_value
    end

    def internal_border_width
      get_parameter :internal_border_width
    end

    def internal_border_width=(new_value)
      set_parameter :internal_border_width, new_value
    end

    def vertical_scroll_bars
      get_parameter :vertical_scroll_bars
    end

    def vertical_scroll_bars=(new_value)
      set_parameter :vertical_scroll_bars, new_value
    end
    
    def scroll_bar_width
      get_parameter :scroll_bar_width
    end

    def scroll_bar_width=(new_value)
      set_parameter :scroll_bar_width, new_value
    end

    def left_fringe
      get_parameter :left_fringe
    end

    def left_fringe=(new_value)
      set_parameter :left_fringe, new_value
    end

    def right_fringe
      get_parameter :right_fringe
    end

    def right_fringe=(new_value)
      set_parameter :right_fringe, new_value
    end

    def menu_bar_lines
      get_parameter :menu_bar_lines
    end

    def menu_bar_lines=(new_value)
      set_parameter :menu_bar_lines, new_value
    end
    
    def tool_bar_lines
      get_parameter :tool_bar_lines
    end

    def tool_bar_lines=(new_value)
      set_parameter :tool_bar_lines, new_value
    end
    
    def line_spacing
      get_parameter :line_spacing
    end

    def line_spacing=(new_value)
      set_parameter :line_spacing, new_value
    end
    
    def minibuffer
      get_parameter :minibuffer
    end

    def minibuffer=(new_value)
      set_parameter :minibuffer, new_value
    end
    
    def unsplittable
      get_parameter :unsplittable
    end

    def unsplittable=(new_value)
      set_parameter :unsplittable, new_value
    end

    def visibility
      get_parameter :visibility
    end

    def visibility=(new_value)
      set_parameter :visibility, new_value
    end
    
    def auto_raise
      get_parameter :auto_raise
    end

    def auto_raise=(new_value)
      set_parameter :auto_raise, new_value
    end

    def auto_lower
      get_parameter :auto_lower
    end

    def auto_lower=(new_value)
      set_parameter :auto_lower, new_value
    end

    def icon_type
      get_parameter :icon_type
    end

    def icon_type=(new_value)
      set_parameter :icon_type, new_value
    end
    
    def icon_name
      get_parameter :icon_name
    end

    def icon_name=(new_value)
      set_parameter :icon_name, new_value
    end
    
    def window_id
      get_parameter :window_id
    end

    def window_id=(new_value)
      set_parameter :window_id, new_value
    end
    
    def outer_window_id
      get_parameter :outer_window_id
    end

    def outer_window_id=(new_value)
      set_parameter :outer_window_id, new_value
    end
    
    def wait_for_wm
      get_parameter :wait_for_wm
    end

    def wait_for_wm=(new_value)
      set_parameter :wait_for_wm, new_value
    end
    
    def cursor_type
      get_parameter :cursor_type
    end

    def cursor_type=(new_value)
      set_parameter :cursor_type, new_value
    end
    
    def background_mode
      get_parameter :background_mode
    end

    def background_mode=(new_value)
      set_parameter :background_mode, new_value
    end

    def tty_color_mode
      get_parameter :tty_color_mode
    end

    def tty_color_mode=(new_value)
      set_parameter :tty_color_mode, new_value
    end

    def screen_gamma
      get_parameter :screen_gamma
    end

    def screen_gamma=(new_value)
      set_parameter :screen_gamma, new_value
    end

    def font
      get_parameter :font
    end

    def font=(new_value)
      set_parameter :font, new_value
    end

    def foreground_color
      get_parameter :foreground_color
    end

    def foreground_color=(new_value)
      set_parameter :foreground_color, new_value
    end

    def background_color
      get_parameter :background_color
    end

    def background_color=(new_value)
      set_parameter :background_color, new_value
    end

    def mouse_color
      get_parameter :mouse_color
    end

    def mouse_color=(new_value)
      set_parameter :mouse_color, new_value
    end

    def cursor_color
      get_parameter :cursor_color
    end

    def cursor_color=(new_value)
      set_parameter :cursor_color, new_value
    end

    def border_color
      get_parameter :border_color
    end

    def border_color=(new_value)
      set_parameter :border_color, new_value
    end

    def scroll_bar_foreground
      get_parameter :scroll_bar_foreground
    end

    def scroll_bar_foreground=(new_value)
      set_parameter :scroll_bar_foreground, new_value
    end

    def scroll_bar_background
      get_parameter :scroll_bar_background
    end

    def scroll_bar_background=(new_value)
      set_parameter :scroll_bar_background, new_value
    end

    ######### end frame parameters
    
  end
  
  # A  proxy to an Emacs window-configuration
  #
  class WindowConfiguration < Proxy

    # _args_ must be of the form (_symbol_, <em>slave =
    # Relisp.default_slave</em>)
    #
    # The _symbol_ argument is considered to be the name of a
    # pre-existing window-configuration in the _slave_ process.  
    #
    def initialize(*args)
      super do 
        raise ArgumentError, "Cannot create WindowConfiguration using 'new' method."
      end
    end
  end

  # A  proxy to an Emacs process
  #
  class Process < Proxy

    # _args_ must be of the form (_symbol_, <em>slave =
    # Relisp.default_slave</em>)
    #
    # The _symbol_ argument is considered to be the name of a
    # pre-existing process in the _slave_ process.  
    #
    def initialize(*args)
      super do 
        raise ArgumentError, "Cannot create Process using 'new' method."
      end
    end
    
    def name
      call_on_self :process_name
    end

    def status
      call_on_self :process_status
    end

    def exit_status
      call_on_self :process_exit_status
    end
  end
  
  # A  proxy to an Emacs Overlay
  #
  class Overlay < Proxy
    
    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (_start_, _end_, <em>buffer = nil</em>, <em>front_advance = nil</em>, <em>rear_advance = nil</em>, <em>slave = Relisp.default_slave</em>)
    # * (_start_, _end_, <em>buffer = nil</em>, <em>slave = Relisp.default_slave</em>)
    # * (_start_, _end_, <em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing overlay in the _slave_ process.  Otherwise a new
    # overlay is created (<tt>make-overlay</tt>).     
    #
    def initialize(*args)
      super do |args|
        @slave.elisp_exec( "(setq #{@elisp_variable} (make-overlay #{args.join(' ')}))" )
      end
    end

    def start
      call_on_self :overlay_start
    end

    def end
      call_on_self :overlay_end
    end

    def buffer
      call_on_self :overlay_buffer
    end

    def delete
      call_on_self :delete_overlay
    end

    def move(new_start, new_end, new_buffer = nil)
      call_on_self :move_overlay, new_start, new_end, new_buffer
    end

    def start=(new_start)
      call_on_self :move_overlay, new_start, self.end
    end

    def end=(new_end)
      call_on_self :move_overlay, self.start, new_end
    end

    private
    
    def get_property(property)
      call_on_self :overlay_get, property
    end

    def set_property(property, new_value)
      call_on_self :overlay_put, property, new_value
    end

    public
    
    def priority
      get_property :priority
    end

    def priority=(new_value)
      set_property :priority, new_value
    end

    def window
      get_property :window
    end

    def window=(new_value)
      set_property :window, new_value
    end

    def category
      get_property :category
    end

    def category=(new_value)
      set_property :category, new_value
    end
   
    def face
      get_property :face
    end

    def face=(new_value)
      set_property :face, new_value
    end

    def mouse_face
      get_property :mouse_face
    end
    
    def mouse_face=(new_value)
      set_property :mouse_face, new_value
    end

    def display
      get_property :display
    end

    def display=(new_value)
      set_property :display, new_value
    end

    def help_echo
      get_property :help_echo
    end

    def help_echo=(new_value)
      set_property :help_echo, new_value
    end

    def modification_hooks
      get_property :help_echo
    end

    def modification_hooks=(new_value)
      set_property :help_echo, new_value
    end

    def insert_in_front_hooks
      get_property :insert_in_front_hooks
    end

    def insert_in_front_hooks=(new_value)
      set_property :insert_in_front_hooks, new_value
    end

    def insert_behind_hooks
      get_property :insert_behind_hooks
    end

    def insert_behind_hooks=(new_value)
      set_property :insert_behind_hooks, new_value
    end

    def invisible
      get_property :invisible
    end

    def invisible=(new_value)
      set_property :invisible, new_value
    end

    def intangible
      get_property :intangible
    end

    def intangible=(new_value)
      set_property :intangible, new_value
    end

    def before_string
      get_property :before_string
    end

    def before_string=(new_value)
      set_property :before_string, new_value
    end

    def after_string
      get_property :after_string
    end

    def after_string=(new_value)
      set_property :after_string, new_value
    end

    def evaporate
      get_property :evaporate
    end

    def evaporate=(new_value)
      set_property :evaporate, new_value
    end

    def local_map
      get_property :local_map
    end

    def local_map=(new_value)
      set_property :local_map, new_value
    end

    def keymap
      get_property :keymap
    end

    def keymap=(new_value)
      set_property :keymap, new_value
    end

  end
  
end

