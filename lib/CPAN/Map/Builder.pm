package CPAN::Map::Builder;

use Moose;
use namespace::autoclean;

use feature 'say';
use FindBin qw();
use File::Path qw(make_path);
require File::Basename;
require File::Spec;
require JSON::XS;
require LWP::Simple;
require IO::Uncompress::Gunzip;
require Text::CSV_XS;
use Digest::MD5 qw(md5_hex);
use Data::Dumper;
use JSON qw( decode_json );     # From CPAN
use Data::Dumper;               # Perl core module


my $default_config = File::Spec->catfile($ENV{HOME}, '.config', 'cpan-map');

has 'refresh_data' => (
    is      => 'rw',
    isa     => 'Bool',
    lazy    => 1,
    default => 0
);

has 'verbose' => (
    is      => 'rw',
    isa     => 'Bool',
    lazy    => 1,
    default => 1
);

has 'config_file' => (
    is      => 'rw',
    isa     => 'Str',
    lazy    => 1,
    default => $default_config,
);

has 'config' => (
    is      => 'rw',
    isa     => 'HashRef',
    lazy    => 1,
    default => sub {
        my $self = shift;
        my $config_file = $self->config_file;
        return {} if !-e $config_file  and  $config_file eq $default_config;
        open my $fh, '<', $config_file;
        local($/);
        my $json_text = <$fh>;
        return JSON::XS::decode_json($json_text);
    },
);

has 'critical_mass' => (
    is      => 'rw',
    isa     => 'Int',
    lazy    => 1,
    default => 10
);

has 'source_data_dir' => (
    is      => 'ro',
    isa     => 'Str',
    lazy    => 1,
    default => File::Spec->catdir(
        File::Basename::dirname($FindBin::Bin), 'source_data'
    ),
);

has 'output_dir' => (
    is      => 'ro',
    isa     => 'Str',
    lazy    => 1,
    default => File::Spec->catdir(
        File::Basename::dirname($FindBin::Bin), 'html'
    ),
);

has 'zoom_scales' => (
    is      => 'ro',
    isa     => 'ArrayRef[Int]',
    lazy    => 1,
    default => sub { [ 3, 4, 5, 6, 8, 10, 20 ] },
    # Note: additions to the zoom-scales list here must be accompanied by
    # corresponding changes to the CSS file
);

has 'plugins_stats_url' => (
    is      => 'rw',
    isa     => 'Str',
    lazy    => 1,
    default => 'http://updates.jenkins-ci.org/update-center.json',
);

has 'plugins_stats' => (
    is      => 'rw',
    isa     => 'Str',
    lazy    => 1,
    default => sub {
        File::Spec->catfile(shift->source_data_dir, 'update-center.json');
    },
);

has 'label_font_path' => (
    is      => 'rw',
    isa     => 'Str',
    lazy    => 1,
    default => sub {
        foreach (
            '/usr/share/fonts/truetype/ttf-liberation/LiberationSans-Regular.ttf',
            '/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf',
        ) {
            return $_ if -e $_;
        }
    },
);

has 'output_writers' => (
    is      => 'rw',
    isa     => 'ArrayRef[Str]',
    lazy    => 1,
    default => sub {
        return [
            'CPAN::Map::WriteJSData',
            'CPAN::Map::WriteMapImages',
            'CPAN::Map::WriteDocRoot',
        ];
    },
);

has 'mass_map' => (
    is      => 'rw',
    isa     => 'HashRef[CPAN::Map::Namespace]',
    lazy    => 1,
    default => sub { {} },
);

has 'plugin_list' => (
    is      => 'rw',
    isa     => 'ArrayRef[CPAN::Map::Plugin]',
    lazy    => 1,
    default => sub { [] },
);

sub plugin_count { scalar @{ shift->plugin_list }; }

has 'label_plugins' => (
    is      => 'rw', 
    isa     => 'Any',
    lazy    => 1,
    default => sub { [] },
);

has 'plugin_index' => (
    is      => 'rw',
    isa     => 'HashRef[Int]',
    lazy    => 1,
    default => sub { {} },
);

has 'maintainers' => (
    is      => 'rw',
    isa     => 'HashRef[CPAN::Map::Maintainer]',
    default => sub { {} }
);

sub maintainer_count { scalar keys %{ shift->maintainers }; }


has 'mod_list_date'    => ( is => 'rw', isa => 'Str' );
has 'slug_of_the_day'  => ( is => 'rw', isa => 'Str' );
has 'plane_rows'       => ( is => 'rw', isa => 'Int' );
has 'plane_cols'       => ( is => 'rw', isa => 'Int' );
has 'label_count'     => ( is => 'rw', isa => 'Int' );
has 'plane'            => ( is => 'rw', isa => 'Ref' );


sub generate {
    my $class = shift;
    my $self  = $class->new(@_);

    if($self->refresh_data) {
        $self->update_source_data;
    }
    $self->list_plugins_by_label;
    $self->map_plugins_to_plane;
    $self->identify_mass_areas;
    $self->load_maintainer_data;
    #$self->load_ratings_data;
    $self->write_output_mappings;
}


sub progress_message {
    my($self, $message) = @_;

    return unless $self->verbose;
    print $message, "\n";
}

sub warning_message {
    my($self, $message) = @_;
    warn "WARNING: <<< $message >>>\n";
}

sub config_item {
    my($self, $key, $default) = @_;

    return $self->config->{$key} // $default;
}

sub update_source_data {
    my($self) = @_;

    my $data_dir = $self->source_data_dir;
    $self->progress_message("Updating source data in $data_dir");

    make_path($data_dir) if not -d $data_dir;

    my($src_url, $dst_file, $status);

    $dst_file = $self->plugins_stats;
    if(!-e $dst_file  or  -M $dst_file > 0.8) {
        $src_url  = $self->plugins_stats_url;
        $status   = LWP::Simple::mirror($src_url, $dst_file);
        die "Status code: $status downloading $src_url"
            unless $status =~ m/^(200|304)$/;
    }
}

sub list_plugins_by_label {
    my $self = shift;

    $self->progress_message('Listing all Jenkins plug-ins');

    open FILE, $self->plugins_stats or die $!;
    my $json = do { local $/; <FILE> };
    $json =~ s/^updateCenter.post\(//;
    $json =~ s/\)\;//;
    
    my $decoded_json = decode_json( $json );
    
    my %plugins = %{ $decoded_json->{'plugins'} };
    
    # Build a big hash of plugins by label
    my %label_plugins = ();
    while(my ($plugin_name, $plugin_body) = each %plugins) {
    	my $plugin = $self->_parse_plugin($plugin_body);
    	if(defined $plugin->labels) {
    	    
            if(ref $plugin->labels eq 'ARRAY') {
                for my $plugin_label (@{$plugin->labels}) {
                    my $existing_plugin = $label_plugins{$plugin_label}->{$plugin->name};
                    if(not defined $existing_plugin) {
                        $label_plugins{$plugin_label}->{$plugin->name} = $plugin;
                    }
                    
                    # if(not exists $label_plugins{$plugin_label}) {
                            # @{$label_plugins{$plugin_label}} = ();
                    # }
                    # push @{$label_plugins{$plugin_label}}, $plugin;
                }
    	   } else {
    	       my $existing_plugin = $label_plugins{$plugin->labels}->{$plugin->name};
               if(not defined $existing_plugin) {
                    $label_plugins{$plugin->labels}->{$plugin->name} = $plugin;
                    $plugin->check_for_main_plugin($plugin);
               }
               # if(not exists $label_plugins{$plugin->labels}) {
                # @{$label_plugins{$plugin->labels}} = ();
               # }
               # push @{$label_plugins{$plugin->labels}}, $plugin;
    	   }
    	}
    	#$self->add_plugin($plugin);
    }
    
    # Create an alphabetical list of plugins and save counts ('mass') of
    # plugins per label
    
    my $mass_map = $self->mass_map;
    my $label_count = 0;
    foreach my $label (sort keys %label_plugins) {
        my $plugins_for_label = delete $label_plugins{$label};
        my @plugins = keys %$plugins_for_label;
    	$label_count++;
    	
    	my $this_label = $mass_map->{$label} = CPAN::Map::Namespace->new(
            name => $label,
            #mass => scalar(@{$label_plugins{$label}}),
            mass => scalar(@plugins),
        );
        
        foreach my $label_name (sort { lc($a) cmp lc($b) } @plugins) {
            my $plugin = $plugins_for_label->{$label_name};
            
            $self->add_plugin($plugin);
        }
    }
    
    $self->label_count($label_count);

    $self->progress_message(" - found " . $self->plugin_count . " plugins");
    $self->progress_message(" - found " . $self->label_count . " labels");
}

sub _parse_plugin {
	my ($self, $plugin_body) = @_;
	return CPAN::Map::Plugin->new(
	   name => $plugin_body->{'name'}, 
	   buildDate => $plugin_body->{'buildDate'},
	   excerpt => $plugin_body->{'excerpt'},
	   title => $plugin_body->{'title'}, 
	   wiki => $plugin_body->{'wiki'}, 
	   version => $plugin_body->{'version'}, 
	   developers => $plugin_body->{'developers'},
	   labels => $plugin_body->{'labels'}
	);
}


sub _parse_plugin_line{
    local($_) = shift;
    return if m{[.]pm(?:[.]gz)?$};
    my($module, $maintainer, $distro_name) = $_ =~ m{
        ^(\S+)                         # Module name
        \s+\S+                         # Version number
        \s+
        (?:[^/]+/){2}                  # Path to maintainer's directory
        ([^/]+)/                       # Maintainer's CPAN-ID
        (?:[^/]+/)*                    # Optional subdirs
        ([^/\s-]+(?:-[^/\s-]+)*)[.-]   # Distribution name
    }x or return;
    $distro_name =~  s{-}{::}g;
    $distro_name =~  s{::\d.+$}{};
    $distro_name =~  s{[.].*$}{};
    $distro_name =~  s{::[vV]\d+$}{};
    my($ns) = split '::', $distro_name, 2;

    return(lc($ns), $distro_name, $maintainer, $module);
}


sub add_plugin {
    my($self, $plugin) = @_;

    my $plugin_list = $self->plugin_list;
    $plugin->index( scalar(@$plugin_list) );
    push @$plugin_list, $plugin;
    $self->plugin_index->{ $plugin->name } = $plugin->index;
}


sub plugin {
    my($self, $i) = @_;

    return unless(defined($i));
    return $self->plugin_list->[$i];
}


sub plugin_by_name {
    my($self, $name) = @_;

    my $i = $self->plugin_index->{$name} or return;
    return $self->plugin($i);
}


sub map_plugins_to_plane {
    my $self = shift;

    $self->progress_message('Mapping all plug-ins into 2D space');

    my $mapper = $self->create_plugin_mapper;

    my($max_row, $max_col, @plane) = (0, 0);
    $self->each_plugin(sub {
        my($plugin) = @_;
        my($row, $col) = $mapper->row_col_from_index($plugin->index);
        $plane[$row][$col] = $plugin->index;
        $plugin->row($row);
        $plugin->col($col);
        $max_row = $row if $row > $max_row;
        $max_col = $col if $col > $max_col;
    });
    $self->plane(\@plane);
    $self->plane_rows($max_row + 1);
    $self->plane_cols($max_col + 1);

    $self->progress_message(
        ' - plane mapping produced ' . $self->plane_rows . ' rows of '
        . $self->plane_cols . ' columns'
    );
}


sub create_plugin_mapper {
    my($self) = @_;

    return CPAN::Map::PlaneMapperHilbert->new(set_size => $self->plugin_count);
}


sub plugin_at {
    my($self, $row, $col) = @_;

    my $plane = $self->plane or return;
    my $r = $plane->[$row] or return;
    my $i = $r->[$col];
    return $self->plugin($i);
}


sub each_plugin {
    my($self, $handler) = @_;

    $handler->($_) foreach ( @{ $self->plugin_list } );
}


sub each_label {
    my($self, $handler) = @_;

    my $mass_map = $self->mass_map;
    $handler->($mass_map->{$_}) foreach (sort keys %$mass_map);
}


sub labels_for_plugin {
    my($self, $plugin) = @_;
    #return $self->mass_map->{ $distro->ns };
    return $self->mass_map->{ $plugin->labels };
}


sub identify_mass_areas {
    my $self = shift;

    $self->progress_message("Identifying 'significant' namespaces");

    # Weed out namespaces smaller than 'critical mass'
    my $mass_map = $self->mass_map;
    my $critical_mass = $self->critical_mass;
    while(my($prefix, $label) = each %$mass_map) {
        if ( $label->mass < $critical_mass ) {
            delete $mass_map->{$prefix};
        }
    }
    
    #say Dumper $mass_map;
    
    # Work out which masses are neighbours (skipping non-critical ones)
    my %neighbour;
    $self->each_plugin(sub {
        my($this_plugin) = @_;
        my $this_name = $this_plugin->name;
        
        # plug-ins with no label
        if( not $this_plugin->labels ) {
            return;
        }
        
        foreach my $label (@{$this_plugin->labels}) {
            my $this_mass = $mass_map->{$label} or return; # == next
            
            $this_mass->update_stats($this_plugin); # for mass center
            
            #$neighbour{ $this_plugin->ns } //= {};  # this is actually needed
            $neighbour{ $label } //= {};  # this is actually needed
            foreach my $look ('right', 'down') {
                my($row1, $col1) = $look eq 'right'
                                 ? ($this_plugin->row, $this_plugin->col + 1)
                                 : ($this_plugin->row + 1, $this_plugin->col);
                my $that_plugin = $self->plugin_at($row1, $col1) or next;
                my $that_prefixes = $that_plugin->labels;
                
                my $contains_label = 0;
                foreach my $that_label (@{$that_prefixes}) {
                    if($that_label eq $label) {
                        $contains_label = 1;
                    }
                }
                if($contains_label) {
                    next;
                } #skip if this plug-in contains the same label
                
                foreach my $that_label (@{$that_prefixes}) {
                    my $that_mass = $mass_map->{$that_label} or next; # not critical
                    if($label ne $that_label) { # each neighbours the other
                        $neighbour{$label}->{$that_label} = 1;
                        $neighbour{$that_label}->{$label} = 1;
                    }
                }
            }
        }
    });

    # Flatten lists of neighbours
    while(my($ns, $value) = each %neighbour ) {
        $neighbour{$ns} = [ sort keys %$value ];
    }
    my @critical_ns = sort keys %neighbour;

    my $count = scalar @critical_ns;
    $self->progress_message(
        " - found $count namespaces containing " . $self->critical_mass .
        " or more plugins"
    );

    # Assign colors to namespaces with critical mass
    $self->progress_message(" - allocating colours to map regions");
    my $colour_map = map_colours({}, \%neighbour, @critical_ns)
        or die "Unable to assign colour map";

    while(my($key, $value) = each %$colour_map) {
        my $mass = $mass_map->{$key};
        $mass->colour($value);
        $mass->finalise_stats();
    }
}


sub map_colours {
    my($map, $neighbour, $ns, @namespaces) = @_;
    no warnings qw(recursion);
    return $map unless $ns;
    my $near = $neighbour->{$ns} or die "no neigbours for $ns!?!";
    my %available = map { $_ => 1 } (1..20);
    foreach my $n ( @$near ) {
        delete $available{ $map->{$n} } if $map->{$n};
    }
    foreach my $try (sort keys %available) {
        $map->{$ns} = $try;
        return $map if map_colours($map, $neighbour, @namespaces);
    }
    delete $map->{$ns};
    return;
}


sub load_maintainer_data {
    my($self) = @_;

    $self->progress_message("Loading maintainer details");

    # Iterate plugin/developers to get more details
    my $count  = 0;
    my $gcount = 0;
    $self->each_plugin(sub {
    	my $plugin = shift;
    	foreach my $developer (values $plugin->developers) {
    		my $id = undef;
            if ( defined $developer->{'developerId'} ) {
                $id = $developer->{'developerId'};
            }
            if(not defined $id) {
            	next;
            }
            $count++;
    		my $name = 'Missing name';
    		if ( defined $developer->{'name'} ) {
    			$name = $developer->{'name'};
    		}
    		my $email = 'Missing e-mail';
            if ( defined $developer->{'email'} ) {
                $email = $developer->{'email'};
            }
            my $maintainer = CPAN::Map::Maintainer->new(
               id => $id, 
               name => $name, 
               email => $email
            );
            
            $self->maintainers->{$id} = $maintainer;
    	}
        #my $cpan_id = shift->maintainer_id;
        #$maint{$cpan_id} //= CPAN::Map::Maintainer->new( id => $cpan_id );
    });

#    while($_ = $z->getline) {
#        my($id, $name, $email) = m{
#            ^alias
#            \s+([\w-]+)                # author ID
#            \s+"(.*?)\s<               # author name
#            (.*?)>                     # email address
#        }x or next;
#        my $maintainer = $maint{$id} or next; # skip if no uploads
#        $maintainer->name($name);
#        $count++;
#        if($email) {
#            $email =~ s{^\s+}{};
#            $email =~ s{\s+$}{};
#            $email =~ s{\s+dot\s+}{.}g;
#            $email =~ s{\s+at\s+}{@};
#            if($email !~ /\s/  and  $email =~ /@/) {
#                $maintainer->email($email);
#                $gcount++;
#            }
#        }
#    }
#
#    $self->maintainers(\%maint);

    $self->progress_message(
        " - found $count 'active' maintainers\n" .
        " - generated $gcount Gravatar IDs"
    );
}


sub each_maintainer {
    my($self, $handler) = @_;

    my $maint = $self->maintainers;
    $handler->($maint->{$_}) foreach (sort keys %$maint);
}


sub load_ratings_data {
    my $self = shift;

    $self->progress_message("Loading ratings details");

    my $csv = Text::CSV_XS->new ({ binary => 1, eol => $/ });
    my $file = $self->ratings_source;
    open my $fh, "<", $file or die "$file: $!";

    my $count = 0;
    while (my $row = $csv->getline($fh)) {
        my($name, $rating, $reviews) = @$row;
        next if length($name) == 0 || $name eq 'distribution';
        $name =~ s/-/::/g;
        my $distro = $self->distro_by_name($name) or next;
        $distro->rating_score($rating);
        $distro->rating_count($reviews);
        $count++;
    }

    $self->progress_message(" - found ratings for $count distributions");
}


sub write_output_mappings {
    my $self = shift;

    my $output_dir = $self->output_dir;
    foreach my $map_class ( @{ $self->output_writers } ) {
        next unless $map_class;  # ignore default keys overridden to undef
        eval "require $map_class";
        die $@ if $@;
        $self->progress_message("Writing output using $map_class");
        $map_class->write($self, $output_dir);
    }
}


__PACKAGE__->meta->make_immutable;



package CPAN::Map::Namespace;

use Moose;
use namespace::autoclean;

require Statistics::Descriptive;

has 'name'     => ( is => 'rw', isa => 'Str' );
has 'mass'     => ( is => 'ro', isa => 'Int' );
has 'colour'   => ( is => 'rw', isa => 'Int' );

has 'label_x'  => ( is => 'rw', isa => 'Num' );
has 'label_y'  => ( is => 'rw', isa => 'Num' );
has 'label_h'  => ( is => 'rw', isa => 'Num' );
has 'label_w'  => ( is => 'rw', isa => 'Num' );

has 'row_stat' => (
    is      => 'ro',
    isa     => 'Statistics::Descriptive::Full',
    lazy    => 1,
    default => sub { Statistics::Descriptive::Full->new(); },
);

has 'col_stat' => (
    is      => 'ro',
    isa     => 'Statistics::Descriptive::Full',
    lazy    => 1,
    default => sub { Statistics::Descriptive::Full->new(); },
);

sub update_stats {
    my($self, $distro) = @_;

    $self->row_stat->add_data($distro->row);
    $self->col_stat->add_data($distro->col);
}

sub finalise_stats {
    my($self) = @_;

    my $stat_x = $self->col_stat;
    $self->label_x( $stat_x->mean );
    $self->label_w( $stat_x->standard_deviation * 1.5 );

    my $stat_y = $self->row_stat;
    $self->label_y( $stat_y->mean );
    $self->label_h( $stat_y->standard_deviation * 1.5 );
}


__PACKAGE__->meta->make_immutable;



package CPAN::Map::Plugin;

use Moose;
use namespace::autoclean;

has 'name'              => ( is => 'ro', isa => 'Str' );
has 'buildDate'         => ( is => 'rw', isa => 'Str' );
has 'excerpt'           => ( is => 'rw', isa => 'Str' );
has 'title'             => ( is => 'rw', isa => 'Str' );
has 'row'               => ( is => 'rw', isa => 'Int' );
has 'col'               => ( is => 'rw', isa => 'Int' );
has 'index'               => ( is => 'rw', isa => 'Int' );
has 'wiki'              => ( is => 'rw', isa => 'Any' );
has 'version'           => ( is => 'rw', isa => 'Any' );
has 'labels'            => ( is => 'rw');
has 'developers'        => ( is => 'rw');
has 'main_module_guess' => ( is => 'rw', isa => 'ArrayRef');


sub main_module {
    my($self) = @_;

    return $self->name; # if $self->is_eponymous;
    my $guess = $self->main_module_guess;
    return $guess->[0];
}


sub check_for_main_plugin {
    my($self, $plugin) = @_;

    my $score = _score_guess($self->name, $plugin);
    if(my $current = $self->main_module_guess) {
        my($guess, $guess_score) = @$current;
        return if $score < $guess_score;
        if($score == $guess_score) {
            return if length($plugin) >= length($guess);
        }
    }
    $self->main_module_guess([$plugin, $score]);
}


sub _score_guess {
    my($distro_name, $module) = @_;

    return 5 if lc($module) eq lc($distro_name);
    return 4 if lc($module) eq lc('app::' . $distro_name);

    if(my($prefix) = $distro_name =~ m{^(.+)(?:-|::)perl$}) {
        return 3 if lc($prefix) eq lc($module);
    }

    (my $bare_distro = lc($distro_name)) =~ s{(?:'|::|_)}{}g;
    (my $bare_module = lc($module))      =~ s{(?:'|::|_)}{}g;
    return 2 if $bare_distro eq $bare_module;

    return 1 if $module =~ m{^\Q$distro_name\E}i;
    return 1 if $module =~ m{\Q$distro_name\E$}i;

    return 0;
}


__PACKAGE__->meta->make_immutable;



package CPAN::Map::Maintainer;

use Moose;
use namespace::autoclean;

use Gravatar::URL qw(gravatar_id);

has 'id'          => ( is => 'ro', isa => 'Str' );
has 'name'        => ( is => 'rw', isa => 'Str', default => 'Missing developer name' );
has 'email'       => ( is => 'rw', isa => 'Str', default => 'Missing developer e-mail' );
has 'gravatar_id' => ( is => 'rw', isa => 'Str', default => 'Missing Gravatar ID' );

before 'email' => sub {
    my $self  = shift;
    my $email = shift or return;
    $self->gravatar_id( gravatar_id($email) );
};


__PACKAGE__->meta->make_immutable;



package CPAN::Map::PlaneMapperHilbert;

use Moose;
use namespace::autoclean;

require Math::PlanePath::HilbertCurve;

has 'set_size'    => ( is => 'ro', isa => 'Int' );

has 'path' => (
    is      => 'ro',
    isa     => 'Math::PlanePath::HilbertCurve',
    lazy    => 1,
    default => sub { Math::PlanePath::HilbertCurve->new },
);

sub row_col_from_index {
    my($self, $i) = @_;

    if($i < 16384) {
        my($x, $y) = $self->path->n_to_xy($i);
        return($x, $y);
    }
    else {
        my($x, $y) = $self->path->n_to_xy($i - 16384);
        return($x, $y + 128);
    }
}


__PACKAGE__->meta->make_immutable;


1;

