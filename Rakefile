begin
  require 'bones'
rescue LoadError
  raise RuntimeError, '### please install the "bones" gem ###'
end

task :default => 'test:run'
ensure_in_path 'lib'
require 'relisp'

Bones do 
  name           'relisp'
  authors        'Don'
  email          'don@ohspite.net'
  url            'https://rubygems.org/gems/relisp'
  summary        'Call ruby from emacs and call elisp from ruby. If you never did you should. These things are fun and fun is good.'
  description    'Call ruby from emacs and call elisp from ruby. If you never did you should. These things are fun and fun is good.'
  version        Relisp::VERSION
#  rubyforge.name 'relisp'
  history_file   'CHANGELOG'
  manifest_file  'Manifest'
  readme_file    'README'
  rdoc.main      'README'
  exclude        %w(tmp$ bak$ ~$ CVS \.svn/ \.git/ \.bzr/ \.bzrignore ^pkg/)
  rdoc.include   %w(README ^lib/ ^bin/ ^ext/ \.txt$ \.rdoc$)
  
  gem.extras[:post_install_message] = <<-MSG
---------------------------
Wait! You're not finished!

The gem is installed, and you can now call elisp from ruby.  But if
you want to call ruby from emacs (and you do, right?) you need to go
into the 'src' directory of this gem and copy 'relisp.el' and/or
'relisp.elc' to your elisp folder (probably something like
'~/.emacs.d/site-lisp' or '~/.elisp').  Then you might want to add the
lines

  (autoload 'relisp-start-slave "relisp" nil t)
  (autoload 'ruby-eval "relisp" nil t)

to your emacs initialization file ('~/.emacs' or ~/.emacs.d/init.el).

If you don't know where to find the files for this gem, run the
command "gem env gemdir".  Or you can download the tarball for this
gem and get the files there.
---------------------------
MSG

#  spec.opts << '--color'
end
