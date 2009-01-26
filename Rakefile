begin
  require 'bones'
  Bones.setup
rescue LoadError
  begin
    load 'tasks/setup.rb'
  rescue LoadError
    raise RuntimeError, '### please install the "bones" gem ###'
  end
end

ensure_in_path 'lib'
require 'relisp'

task :default => 'test'

PROJ.name           = 'relisp'
PROJ.authors        = 'Don'
PROJ.email          = 'don@ohspite.net'
PROJ.url            = 'relisp.rubyforge.org'
PROJ.summary        = 'Call ruby from emacs and call elisp from ruby. If you never did you should. These things are fun and fun is good.'
PROJ.description    = 'Call ruby from emacs and call elisp from ruby. If you never did you should. These things are fun and fun is good.'
PROJ.version        = Relisp::VERSION
PROJ.rubyforge.name = 'relisp'
PROJ.history_file   = 'CHANGELOG'
PROJ.manifest_file  = 'Manifest'
PROJ.readme_file    = 'README'
PROJ.rdoc.main      = 'README'
PROJ.exclude        = %w(tmp$ bak$ ~$ CVS \.svn/ \.git/ \.bzr/ \.bzrignore ^pkg/)
PROJ.rdoc.include   = %w(README ^lib/ ^bin/ ^ext/ \.txt$ \.rdoc$)

PROJ.gem.extras[:post_install_message] = <<-MSG
---------------------------
Wait! You're not finished!

The gem is installed, and you can now call elisp from ruby.  But if
you want to call ruby from emacs (and you do, right?) you need to go
into the 'src' directory of this gem and copy 'relisp.el' and/or
'relisp.elc' to your elisp folder (probably '~/.elisp').  Then you
might want to add the line "(require 'relisp)" to your emacs
initialization file (probably '~/.emacs').

If you don't know where to find the files for this gem, run the
command "gem env gemdir".  Or you can download the tarball for this
gem and get the files there.
---------------------------
MSG

PROJ.spec.opts << '--color'

