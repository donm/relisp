module Relisp

  ### Editing Types
  ### Buffer Type::         The basic object of editing.
  # Marker Type::         A position in a buffer.
  # Window Type::         Buffers are displayed in windows.
  # Frame Type::		Windows subdivide frames.
  # Window Configuration Type::   Recording the way a frame is subdivided.
  # Frame Configuration Type::    Recording the status of all frames.
  # Process Type::        A process running on the underlying OS.
  # Stream Type::         Receive or send characters.
  # Keymap Type::         What function a keystroke invokes.
  # Overlay Type::        How an overlay is represented.    case elisp_type

  class Buffer
    def self.from_elisp(object)
      new(object[:variable], object[:slave])
    end

    def initialize(old_elisp_variable, slave)
      @elisp_variable = slave.get_permament_variable(old_elisp_variable)
      @slave = slave
    end

    def name
      @slave.elisp_eval "(buffer-name #{@elisp_variable})"
    end

    def to_elisp
      @elisp_variable
    end
  end

end
