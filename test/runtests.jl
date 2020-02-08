using Test

using Colonies

@testset "Directories" begin

    @test extractid("123-456") == "123-456"
    @test extractid("colony-123-456.png") == "123-456"

end
