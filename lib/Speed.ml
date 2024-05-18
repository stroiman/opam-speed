module Domain = Speed_domain
module Assertions = Speed_assertions
module Runner = Speed_runner
module Metadata = Speed_metadata

module Dsl = struct
  module List = Speed_dsl_list

  module Effect = struct
    include Speed_dsl_effect
    module Simple = Speed_dsl_effect_simple
  end
end

type metadata = Metadata.t = ..
