function encode(s::AbstractString)
    m = rand(Mask, collect(generate_masks(1:4, 3)))
    f = rand(StateFilter, collect(generate_state_filters(false)))
    encode(s, m, f)
end

function encode(s::AbstractString, mask::Mask, filter::StateFilter)


    context = CommonwealthContext()

    # a 7 x n matrix where n = nb of characters in string s
    A = hcat((digits(UInt8(c), base=2) for c in s)...)
    # think about this later!!!

end
