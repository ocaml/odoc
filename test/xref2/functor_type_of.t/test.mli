module Make (T : sig end) : sig type included end
module Named : module type of Make

module Applicant : sig end

module Applied : module type of Named(Applicant)
