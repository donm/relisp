#--
# Copyright (C) 2009, 2010 Don March
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

  # A proxy to an Emacs buffer.
  #
  class Buffer < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (_string_, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing buffer in the _slave_ process.  Otherwise a new
    # buffer is created (<tt>generate-new-buffer</tt>).  The name is
    # _string_, if given, and a variant of "relisp-buffer" otherwise.
    #
    def initialize(*args)
      super do |sargs|
        name = sargs[0] ? sargs[0] : "relisp-buffer"
        raise ArgumentError unless name.kind_of?(String)
        @slave.elisp_exec( "(setq #{@elisp_variable} (generate-new-buffer #{name.to_elisp}))" )
      end
    end

    private

    def eval_in_buffer(code)
      @slave.elisp_eval <<EOM
(save-excursion
  (set-buffer #{@elisp_variable})
  #{code})
EOM
    end

    public

    def set
      @slave.set_buffer(@elisp_variable.value)
    end
    
    ##
    # Return the name of Buffer, as a string (<tt>buffer-name</tt>).
    #
    elisp_alias :name, :buffer_name

    # Change current buffer's name to _newname_ (a string).  If
    # _unique_ is nil, it is an error if a buffer named _newname_
    # already exists.  If _unique_ is non-nil, come up with a new name
    # using <tt>generate-new-buffer-name'</tt>
    # (<tt>rename-buffer</tt>).
    #
    def rename(newname, unique = false)
      eval_in_buffer "(rename-buffer #{newname.to_elisp} #{unique.to_elisp})"
    end

    alias name= rename

    #
    # Return name of file that the Buffer is visiting, or nil if none
    # (<tt>buffer-file-name</tt>).
    #
    elisp_alias :filename, :buffer_file_name

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

    def set_modified(flag=true)
      eval_in_buffer "(set-buffer-modified-p #{flag.to_elisp})"
    end

    def modified=(flag)
      set_modified(flag)
    end

    ##
    elisp_alias :modified?, :buffer_modified_p

    ##
    elisp_alias :modified_tick, :buffer_modified_tick

    ##
    elisp_alias :chars_modified_tick, :buffer_chars_modified_tick

    def read_only?
      eval_in_buffer "buffer-read-only"
    end

    def read_only=(flag)
      eval_in_buffer "(setq buffer-read-only #{flag.to_elisp})"
    end

    def bury
      call_on_self :bury_buffer
      if @slave.elisp_eval "(equal (current-buffer) #{self.to_elisp})"
        @slave.elisp_exec "(switch-to-buffer (other-buffer))"
      end
    end

    def kill
      raise if modified?
      call_on_self :kill_buffer
    end

    def kill!
      set_modified(false)
      kill
    end

    ##
    elisp_alias :alive?, :buffer_live_p

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

    ##
    elisp_alias :size, :buffer_size

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

    ##
    elisp_alias :window, :get_buffer_window

    def window=(new_window)
      new_window.buffer = self
    end

    ##
    elisp_alias :windows, :get_buffer_window_list

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

    def ==(buffer2)
      @slave.elisp_eval "(equal #{to_elisp} #{buffer2.to_elisp})"
    end

    def method_missing(method, *args)
      @slave.save_excursion do 
        set
        @slave.send(method, *args)
      end
    end
  end

  # A proxy to an Emacs marker.
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

    ##
    elisp_alias :position, :marker_position

    ##
    elisp_alias :buffer, :marker_buffer

    ##
    elisp_alias :insertion_type, :marker_insertion_type

    def insertion_type=(type)
      @slave.elisp_eval( "(set-marker-insertion-type #{@elisp_variable} #{type.to_elisp})" )
    end

    def set(new_position, new_buffer=nil)
      @slave.elisp_exec( "(set-marker #{@elisp_variable} #{new_position} #{new_buffer})" )
    end

    alias move set
    alias position= set
  end

  # A proxy to an Emacs window
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
        raise ArgumentError, "Cannot create Window proxy without existing elisp variable."
      end
    end

    private

    def eval_in_window(code)
      @slave.elisp_eval <<EOM
(with-selected-window #{@elisp_variable}
  #{code})
EOM
    end

    public

    ##
    elisp_alias "split(size=nil, horizontal=false)", "split_window, size, horizontal"

    def split_horizontally(size=nil)
      split(size, true)
    end

    def split_vertically(size=nil)
      split(size, false)
    end

    ##
    elisp_alias :alive?, :window_live_p

    ##
    elisp_alias :delete, :delete_window

    ##
    elisp_alias :delete_others, :delete_other_windows

    ##
    elisp_alias :select, :select_window

    ##
    elisp_alias :buffer, :window_buffer

    ##
    elisp_alias 'buffer=(new_buffer)', 'set_window_buffer, new_buffer'

    ##
    elisp_alias :dedicated, :window_dedicated_p

    ##
    elisp_alias 'dedicated=(flag)', 'set_window_dedicated_p, flag'

    ##
    elisp_alias :point, :window_point

    ##
    elisp_alias 'point=(position)', 'set_window_point, position'

    ##
    elisp_alias :start, :window_start
#   eval_in_window "(window-start)"

    ##
    elisp_alias 'start=(position)', 'set_window_start, position'

    ##
    elisp_alias :end , :window_end

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

    ##
    elisp_alias :vscroll, :window_vscroll

    ##
    elisp_alias 'vscroll_in_pixels', 'window_vscroll, true'

    ##
    elisp_alias 'vscroll=(lines)', 'set_window_vscroll, lines'

    ##
    elisp_alias 'vscroll_in_pixels=(pixels)', 'set_window_vscroll, pixels, true'

    def scroll_left(count=nil)
      eval_in_window "(scroll-left #{count.to_elisp})"
    end

    def scroll_right(count=nil)
      eval_in_window "(scroll-right #{count.to_elisp})"
    end

    ##
    elisp_alias :hscroll, :window_hscroll

    ##
    elisp_alias "hscroll=(columns)", "set_window_hscroll, columns"

    ##
    elisp_alias :height, :window_height

    ##
    elisp_alias :body_height, :window_body_height

    ##
    elisp_alias :width, :window_width

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
      eval_in_window "(enlarge-window #{vertical.to_elisp})" unless vertical == 0
      eval_in_window "(enlarge-window-horizontally #{horizontal.to_elisp})" unless horizontal == 0
    end

    def shrink(vertical, horizontal=0)
      eval_in_window "(shrink-window #{vertical.to_elisp})" unless vertical == 0
      eval_in_window "(shrink-window-horizontally #{horizontal.to_elisp})" unless horizontal == 0
    end

    ##
    elisp_alias :frame, :window_frame

    def ==(window2)
      @slave.elisp_eval "(equal #{to_elisp} #{window2.to_elisp})"
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
      super do |sargs|
        hash = sargs[0]
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

    ##
    elisp_alias :delete, :delete_frame

    ##
    elisp_alias :alive?, :frame_live_p

    ##
    elisp_alias :selected_window, :frame_selected_window

    ##
    elisp_alias "selected_window=(window)", "set_frame_selected_window, window"

    ##
    elisp_alias :focus, :select_frame_set_input_focus

    ##
    elisp_alias :select, :select_frame

    ##
    elisp_alias :visible?, :frame_visible_p

    ##
    elisp_alias :show, :make_frame_visible

    ##
    elisp_alias :hide, :make_frame_invisible

    ##
    elisp_alias :iconify, :inconify_frame

    ##
    elisp_alias :raise, :raise_frame

    ##
    elisp_alias :lower, :lower_frame

    private
    
    ##
    elisp_alias 'get_parameter(parameter)', 'frame_parameter, parameter'

    def set_parameter(parameter, new_value)
      alist = Cons.new(Cons.new(parameter, new_value), nil)
      call_on_self :modify_frame_parameters, alist
    end

    public

    ######### begin frame parameters

    def self.param_reader name
      class_eval <<-EOM
      def #{name}
          get_parameter :#{name}
        end
      EOM
    end

    def self.param_writer name
      class_eval <<-EOM
      def #{name}=(new_value)
          set_parameter :#{name}, new_value
        end
      EOM
    end

    # all of this could be a lot slicker, but for the sake of RDoc
    # it's not

    ##
    param_reader :display

    ##
    # :method: display=
    # :call-seq: 
    #   display=(new_value)
    param_writer :display

    ##
    param_reader :display_type

    ##
    # :method: display_type=
    # :call-seq: 
    #   display_type=(new_value)
    param_writer :display_type

    ##
    param_reader :title

    ##
    # :method: title=
    # :call-seq: 
    #   title=(new_value)
    param_writer :title

    ##
    param_reader :name

    ##
    # :method: name=
    # :call-seq: 
    #   name=(new_value)
    param_writer :name

    ##
    param_reader :left

    ##
    # :method: left=
    # :call-seq: 
    #   left=(new_value)
    param_writer :left

    ##
    param_reader :top

    ##
    # :method: top=
    # :call-seq: 
    #   top=(new_value)
    param_writer :top

    ##
    param_reader :height

    ##
    # :method: height=
    # :call-seq: 
    #   height=(new_value)
    param_writer :height

    ##
    param_reader :width

    ##
    # :method: width=
    # :call-seq: 
    #   width=(new_value)
    param_writer :width

    ##
    param_reader :fullscreen

    ##
    # :method: fullscreen=
    # :call-seq: 
    #   fullscreen=(new_value)
    param_writer :fullscreen

    ##
    param_reader :border_width

    ##
    # :method: border_width=
    # :call-seq: 
    #   border_width=(new_value)
    param_writer :border_width

    ##
    param_reader :internal_border_width

    ##
    # :method: internal_border_width=
    # :call-seq: 
    #   internal_border_width=(new_value)
    param_writer :internal_border_width

    ##
    param_reader :vertical_scroll_bars

    ##
    # :method: vertical_scroll_bars=
    # :call-seq: 
    #   vertical_scroll_bars=(new_value)
    param_writer :vertical_scroll_bars
    
    ##
    param_reader :scroll_bar_width

    ##
    # :method: scroll_bar_width=
    # :call-seq: 
    #   scroll_bar_width=(new_value)
    param_writer :scroll_bar_width

    ##
    param_reader :left_fringe

    ##
    # :method: left_fringe=
    # :call-seq: 
    #   left_fringe=(new_value)
    param_writer :left_fringe

    ##
    param_reader :right_fringe

    ##
    # :method: right_fringe=
    # :call-seq: 
    #   right_fringe=(new_value)
    param_writer :right_fringe

    ##
    param_reader :menu_bar_lines

    ##
    # :method: menu_bar_lines=
    # :call-seq: 
    #   menu_bar_lines=(new_value)
    param_writer :menu_bar_lines
    
    ##
    param_reader :tool_bar_lines

    ##
    # :method: tool_bar_lines=
    # :call-seq: 
    #   tool_bar_lines=(new_value)
    param_writer :tool_bar_lines
    
    ##
    param_reader :line_spacing

    ##
    # :method: line_spacing=
    # :call-seq: 
    #   line_spacing=(new_value)
    param_writer :line_spacing
    
    ##
    param_reader :minibuffer

    ##
    # :method: minibuffer=
    # :call-seq: 
    #   minibuffer=(new_value)
    param_writer :minibuffer
    
    ##
    param_reader :unsplittable

    ##
    # :method: unsplittable=
    # :call-seq: 
    #   unsplittable=(new_value)
    param_writer :unsplittable

    ##
    param_reader :visibility

    ##
    # :method: visibility=
    # :call-seq: 
    #   visibility=(new_value)
    param_writer :visibility
    
    ##
    param_reader :auto_raise

    ##
    # :method: auto_raise=
    # :call-seq: 
    #   auto_raise=(new_value)
    param_writer :auto_raise

    ##
    param_reader :auto_lower

    ##
    # :method: auto_lower=
    # :call-seq: 
    #   auto_lower=(new_value)
    param_writer :auto_lower

    ##
    param_reader :icon_type

    ##
    # :method: icon_type=
    # :call-seq: 
    #   icon_type=(new_value)
    param_writer :icon_type
    
    ##
    param_reader :icon_name

    ##
    # :method: icon_name=
    # :call-seq: 
    #   icon_name=(new_value)
    param_writer :icon_name
    
    ##
    param_reader :window_id

    ##
    # :method: window_id=
    # :call-seq: 
    #   window_id=(new_value)
    param_writer :window_id
    
    ##
    param_reader :outer_window_id

    ##
    # :method: outer_window_id=
    # :call-seq: 
    #   outer_window_id=(new_value)
    param_writer :outer_window_id
    
    ##
    param_reader :wait_for_wm

    ##
    # :method: wait_for_wm=
    # :call-seq: 
    #   wait_for_wm=(new_value)
    param_writer :wait_for_wm
    
    ##
    param_reader :cursor_type

    ##
    # :method: cursor_type=
    # :call-seq: 
    #   cursor_type=(new_value)
    param_writer :cursor_type
    
    ##
    param_reader :background_mode

    ##
    # :method: background_mode=
    # :call-seq: 
    #   background_mode=(new_value)
    param_writer :background_mode

    ##
    param_reader :tty_color_mode

    ##
    # :method: tty_color_mode=
    # :call-seq: 
    #   tty_color_mode=(new_value)
    param_writer :tty_color_mode

    ##
    param_reader :screen_gamma

    ##
    # :method: screen_gamma=
    # :call-seq: 
    #   screen_gamma=(new_value)
    param_writer :screen_gamma

    ##
    param_reader :font

    ##
    # :method: font=
    # :call-seq: 
    #   font=(new_value)
    param_writer :font

    ##
    param_reader :foreground_color

    ##
    # :method: foreground_color=
    # :call-seq: 
    #   foreground_color=(new_value)
    param_writer :foreground_color

    ##
    param_reader :background_color

    ##
    # :method: background_color=
    # :call-seq: 
    #   background_color=(new_value)
    param_writer :background_color

    ##
    param_reader :mouse_color

    ##
    # :method: mouse_color=
    # :call-seq: 
    #   mouse_color=(new_value)
    param_writer :mouse_color

    ##
    param_reader :cursor_color

    ##
    # :method: cursor_color=
    # :call-seq: 
    #   cursor_color=(new_value)
    param_writer :cursor_color

    ##
    param_reader :border_color

    ##
    # :method: border_color=
    # :call-seq: 
    #   border_color=(new_value)
    param_writer :border_color

    ##
    param_reader :scroll_bar_foreground

    ##
    # :method: scroll_bar_foreground=
    # :call-seq: 
    #   scroll_bar_foreground=(new_value)
    param_writer :scroll_bar_foreground

    ##
    param_reader :scroll_bar_background

    ##
    # :method: scroll_bar_background=
    # :call-seq: 
    #   scroll_bar_background=(new_value)
    param_writer :scroll_bar_background

    ######### end frame parameters
    
  end
  
  # A proxy to an Emacs window-configuration
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
        raise ArgumentError, "Cannot create WindowConfiguration proxy without existing elisp variable."
      end
    end
  end

  # A proxy to an Emacs process
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
        raise ArgumentError, "Cannot create Process proxy without existing elisp variable."
      end
    end

    ##
    elisp_alias :name, :process_name

    ##
    elisp_alias :status, :process_status

    ##
    elisp_alias :exit_status, :process_exit_status
  end
  
  # A proxy to an Emacs Overlay
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
      super do |sargs|
        @slave.elisp_exec( "(setq #{@elisp_variable} (make-overlay #{sargs.join(' ')}))" )
      end
    end

    ##
    elisp_alias :start, :overlay_start

    ##
    elisp_alias :end, :overlay_end

    ##
    elisp_alias :buffer, :overlay_buffer

    ##
    elisp_alias :delete, :delete_overlay

    ##
    elisp_alias "move(new_start, new_end, new_buffer = nil)", "move_overlay, new_start, new_end, new_buffer"

    ##
    elisp_alias "start=(new_start)", "move_overlay, new_start, self.end"

    ##
    elisp_alias "end=(new_end)", "move_overlay, self.start, new_end"

    private
    
    def get_property(property)
      call_on_self :overlay_get, property
    end

    def set_property(property, new_value)
      call_on_self :overlay_put, property, new_value
    end

    public
    
    def self.prop_reader name
      class_eval <<-EOM
      def #{name}
          get_property :#{name}
        end
      EOM
    end

    def self.prop_writer name
      class_eval <<-EOM
      def #{name}=(new_value)
          set_property :#{name}, new_value
        end
      EOM
    end

    ##
    prop_reader :priority
    
    ##
    # :method: priority=
    # :call-seq:
    #   priority=(new_value)
    prop_writer :priority

    ##
    prop_reader :window
    
    ##
    # :method: window=
    # :call-seq:
    #   window=(new_value)
    prop_writer :window

    ##
    prop_reader :category
    
    ##
    # :method: category=
    # :call-seq:
    #   category=(new_value)
    prop_writer :category
   
    ##
    prop_reader :face
    
    ##
    # :method: face=
    # :call-seq:
    #   face=(new_value)
    prop_writer :face

    ##
    prop_reader :mouse_face
    
    ##
    # :method: mouse_face=
    # :call-seq:
    #   mouse_face=(new_value)
    prop_writer :mouse_face

    ##
    prop_reader :display
    
    ##
    # :method: display=
    # :call-seq:
    #   display=(new_value)
    prop_writer :display

    ##
    prop_reader :help_echo
    
    ##
    # :method: help_echo=
    # :call-seq:
    #   help_echo=(new_value)
    prop_writer :help_echo

    ##
    prop_reader :modification_hooks
    
    ##
    # :method: modification_hooks=
    # :call-seq:
    #   modification_hooks=(new_value)
    prop_writer :modification_hooks

    ##
    prop_reader :insert_in_front_hooks
    
    ##
    # :method: insert_in_front_hooks=
    # :call-seq:
    #   insert_in_front_hooks=(new_value)
    prop_writer :insert_in_front_hooks

    ##
    prop_reader :insert_behind_hooks
    
    ##
    # :method: insert_behind_hooks=
    # :call-seq:
    #   insert_behind_hooks=(new_value)
    prop_writer :insert_behind_hooks

    ##
    prop_reader :invisible
    
    ##
    # :method: invisible=
    # :call-seq:
    #   invisible=(new_value)
    prop_writer :invisible

    ##
    prop_reader :intangible
    
    ##
    # :method: intangible=
    # :call-seq:
    #   intangible=(new_value)
    prop_writer :intangible

    ##
    prop_reader :before_string
    
    ##
    # :method: before_string=
    # :call-seq:
    #   before_string=(new_value)
    prop_writer :before_string

    ##
    prop_reader :after_string
    
    ##
    # :method: after_string=
    # :call-seq:
    #   after_string=(new_value)
    prop_writer :after_string

    ##
    prop_reader :evaporate
    
    ##
    # :method: evaporate=
    # :call-seq:
    #   evaporate=(new_value)
    prop_writer :evaporate

    ##
    prop_reader :local_map
    
    ##
    # :method: local_map=
    # :call-seq:
    #   local_map=(new_value)
    prop_writer :local_map

    ##
    prop_reader :keymap
    
    ##
    # :method: keymap=
    # :call-seq:
    #   keymap=(new_value)
    prop_writer :keymap

  end
  
end

