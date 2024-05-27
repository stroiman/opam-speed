module Domain = Speed_domain
module Assertions = Speed_assertions
module Runner = Speed_runner
module Metadata = Speed_metadata

module Dsl = struct
  module Helpers = struct
    let focus (t : ?focus:bool -> 'a) x = (t ~focus:true) x

    let with_metadata metadata (t : ?metadata:Domain.Metadata.t list -> 'a) x =
      (t ~metadata) x
  end

  module List = struct
    include Speed_dsl_list
    include Helpers
  end

  module Effect = struct
    include Speed_dsl_effect
    include Helpers

    module Simple = struct
      include Speed_dsl_effect_simple
      include Helpers
    end
  end
end

type metadata = Metadata.t = ..
